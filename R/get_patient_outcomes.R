#' Get Patient Outcomes Data
#'
#' Retrieves patient outcome data for early and non-early PVI cohorts from an OMOP CDM database.
#'
#' @param conn A DBI connection object to the database
#' @param schema Character string specifying the schema name
#' @param early_cohort_id The cohort definition ID for early PVI patients (required, no default)
#' @param non_early_cohort_id The cohort definition ID for non-early PVI patients (required, no default)
#' @param outcome_window Days after cohort start to begin tracking outcomes (default: 183)
#' @param end_date End date for follow-up period if death not recorded (default: "2022-12-31")
#'
#' @return A data frame containing patient outcomes data
#' @export
#' @importFrom DBI dbGetQuery
get_patient_outcomes <- function(conn,
                                 schema,
                                 early_cohort_id,
                                 non_early_cohort_id,
                                 outcome_window = 183,
                                 end_date = "2022-12-31") {

  # Format schema with proper dot notation if not already present
  if (!grepl("\\.", schema)) {
    schema_prefix <- paste0(schema, ".")
  } else {
    schema_prefix <- ""
  }

  # SQL query
  query <- paste0("WITH cohort_patients AS (
    -- Get ALL patients from both cohorts with their start dates
    SELECT
        subject_id as person_id,
        cohort_start_date,
        CASE
            WHEN cohort_definition_id = ", early_cohort_id, " THEN 'Early PVI'
            WHEN cohort_definition_id = ", non_early_cohort_id, " THEN 'Non-Early PVI'
        END as cohort_group,
        CASE
            WHEN cohort_definition_id = ", early_cohort_id, " THEN DATEADD(day, ", outcome_window, ", cohort_start_date)
            WHEN cohort_definition_id = ", non_early_cohort_id, " THEN DATEADD(day, ", outcome_window, ", cohort_start_date)
        END as outcome_start_date
    FROM ", schema_prefix, "results_cohort
    WHERE cohort_definition_id IN (", early_cohort_id, ", ", non_early_cohort_id, ")
),

clti_events AS (
    SELECT
        cp.person_id,
        MIN(co.condition_start_date) as clti_date
    FROM cohort_patients cp
    LEFT JOIN ", schema_prefix, "condition_occurrence co
        ON cp.person_id = co.person_id
    WHERE co.condition_concept_id IN (
        4326561,318712,443729,321052,4325344,321822,605283,605282,4141106,444264
    )
    AND co.condition_start_date > cp.outcome_start_date
    GROUP BY cp.person_id
),

pvi_events AS (
    SELECT
        cp.person_id,
        MIN(event_date) as pvi_date
    FROM cohort_patients cp
    LEFT JOIN (
        SELECT person_id, procedure_date as event_date
        FROM ", schema_prefix, "procedure_occurrence
        WHERE procedure_concept_id IN (
            40756934,40756856,40756946,2107761,2107774,40756958,2107775,40756872,
            2731828,2107741,40757029,2732487,40757057,2732271,2732478,2732469,
            2732496,2732505,2732514,2731567,2733065,2732523,2732728,2732719,
            2107752,2732225,2732031,40757117,2731573,2732261,2732279,2809385,
            2732270,2868339,2732252,2804374,2854686,2732253,2732262,2108036,
            2107754,2732226,2732269,2732278,2732582,2732217,2731835,2732268,
            2737032,40757128,2732591,2107760,2732486,2737041,2737513,2732224,
            2732755,2732251,2732477,2731857
        )
        UNION ALL
        SELECT person_id, device_exposure_start_date as event_date
        FROM ", schema_prefix, "device_exposure
        WHERE device_concept_id IN (2615752,2615751,2615864)
    ) combined_events ON cp.person_id = combined_events.person_id
    WHERE event_date > cp.outcome_start_date
    GROUP BY cp.person_id
),

amputation_events AS (
    SELECT
        cp.person_id,
        MIN(po.procedure_date) as amputation_date
    FROM cohort_patients cp
    LEFT JOIN ", schema_prefix, "procedure_occurrence po
        ON cp.person_id = po.person_id
    WHERE po.procedure_concept_id IN (
        4217482,4002166,3216236,3332986,4054498,3229010,37109895,3369581,36675624,
        36675625,3292024,37109581,37109845,3282074,37115743,36717437,3242376,
        3288670,3387945,3353030,3229607,4219032,37109582,3300332,36715397,3340047,
        37118455,3176310,36715395,3393829,3282425,3241791,2105451,3338258,3315344,
        4177620,3186706,37204044,3418768,4226945,3529146,3529147,3101815,3086020,
        3118156,3149876,3101809,3081993,3157298,3118164,3118158,3157573,3159869,
        3159870,3567436,3537021,3531662,4107439,4108566,4078401,3101819,40570136,
        40527679,40309384,40383478,40309388,3118159,40348021,40309376,4195136,
        4264289,36675618,4338257,4266202,4108565,4159766,4302020,4272232,2105446,
        2105448,2105449,2105209,2105211,2105222
    )
    AND po.procedure_date > cp.outcome_start_date
    GROUP BY cp.person_id
)

-- Final query joining ALL patients with their outcomes
SELECT
    cp.person_id,
    cp.cohort_group,
    cp.cohort_start_date as index_date,
    cp.outcome_start_date,
    COALESCE(d.death_date, '", end_date, "') as end_date,
    DATEDIFF(day, cp.cohort_start_date, COALESCE(d.death_date, '", end_date, "')) as follow_up_days,

    -- CLTI outcome
    clti.clti_date,
    CASE WHEN clti.clti_date IS NOT NULL THEN 1 ELSE 0 END as had_clti,
    CASE WHEN clti.clti_date IS NOT NULL
         THEN DATEDIFF(day, cp.cohort_start_date, clti.clti_date)
         ELSE NULL
    END as days_to_clti,

    -- PVI outcome
    pvi.pvi_date,
    CASE WHEN pvi.pvi_date IS NOT NULL THEN 1 ELSE 0 END as had_pvi,
    CASE WHEN pvi.pvi_date IS NOT NULL
         THEN DATEDIFF(day, cp.cohort_start_date, pvi.pvi_date)
         ELSE NULL
    END as days_to_pvi,

    -- Amputation outcome
    amp.amputation_date,
    CASE WHEN amp.amputation_date IS NOT NULL THEN 1 ELSE 0 END as had_amputation,
    CASE WHEN amp.amputation_date IS NOT NULL
         THEN DATEDIFF(day, cp.cohort_start_date, amp.amputation_date)
         ELSE NULL
    END as days_to_amputation
FROM cohort_patients cp
LEFT JOIN ", schema_prefix, "death d ON cp.person_id = d.person_id
LEFT JOIN clti_events clti ON cp.person_id = clti.person_id
LEFT JOIN pvi_events pvi ON cp.person_id = pvi.person_id
LEFT JOIN amputation_events amp ON cp.person_id = amp.person_id")

# Execute the query
all_patients_df <- DBI::dbGetQuery(conn, query)

return(all_patients_df)
}
