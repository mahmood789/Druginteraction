# app.R

# Load necessary libraries
# If you don't have these installed, run:
# install.packages(c("shiny", "shinythemes", "DT", "dplyr", "tidyr", "visNetwork", "jsonlite"))
library(shiny)
library(shinythemes)
library(DT) # For interactive tables
library(dplyr) # For data manipulation
library(tidyr) # For data tidying
library(visNetwork) # For interactive network graphs
library(jsonlite) # For handling JSON data (e.g., for visNetwork)

# --- Data for Drug Interactions ---
# This is a conceptual dataset. In a real application, this would be much larger
# and ideally sourced from a comprehensive drug interaction database.
# Added Severity and DrugClass for better visualization and challenge mode.

drug_interactions_data <- tribble(
  ~Drug1, ~Drug1Class, ~Drug2, ~Drug2Class, ~InteractionType, ~Mechanism, ~ClinicalSignificance, ~Management, ~Severity,
  "Lisinopril", "Cardiology", "Spironolactone", "Cardiology", "Pharmacodynamic", "Hyperkalemia risk due to dual potassium-sparing effect.", "Potentially life-threatening hyperkalemia, especially in renal impairment.", "Monitor serum potassium closely, especially when initiating or increasing dose. Avoid co-administration if possible.", "Major",
  "Warfarin", "Cardiology", "Amiodarone", "Cardiology", "Pharmacokinetic (CYP2C9 inhibition)", "Amiodarone inhibits warfarin metabolism, increasing INR and bleeding risk.", "Significant increase in bleeding risk. Can be severe.", "Reduce warfarin dose by 30-50% when initiating amiodarone. Monitor INR frequently (daily/every other day) until stable.", "Major",
  "Warfarin", "Cardiology", "Ibuprofen", "Non-Cardiology", "Pharmacodynamic", "Increased bleeding risk due to antiplatelet effect of NSAIDs and anticoagulant effect of warfarin.", "Increased risk of gastrointestinal bleeding and other hemorrhagic events.", "Avoid co-administration if possible. If necessary, use lowest effective dose of NSAID for shortest duration. Monitor for bleeding.", "Major",
  "Metoprolol", "Cardiology", "Verapamil", "Cardiology", "Pharmacodynamic", "Additive negative chronotropic and inotropic effects.", "Risk of severe bradycardia, AV block, and heart failure exacerbation.", "Avoid co-administration in patients with pre-existing bradycardia, AV block, or heart failure. Monitor heart rate and BP closely. Consider alternative agents.", "Major",
  "Digoxin", "Cardiology", "Amiodarone", "Cardiology", "Pharmacokinetic (P-gp inhibition)", "Amiodarone increases digoxin levels by inhibiting its efflux.", "Risk of digoxin toxicity (nausea, vomiting, arrhythmias, visual disturbances).", "Reduce digoxin dose by 30-50% when initiating amiodarone. Monitor digoxin levels and clinical signs of toxicity.", "Major",
  "Simvastatin", "Cardiology", "Amlodipine", "Cardiology", "Pharmacokinetic (CYP3A4 inhibition)", "Amlodipine (weak inhibitor) can increase simvastatin levels, increasing myopathy risk.", "Increased risk of myopathy and rhabdomyolysis.", "Limit simvastatin dose to 20 mg/day if co-administered with amlodipine. Consider alternative statin.", "Moderate",
  "Furosemide", "Cardiology", "Gentamicin", "Non-Cardiology", "Pharmacodynamic", "Additive ototoxicity.", "Increased risk of hearing loss.", "Avoid co-administration if possible. If necessary, monitor hearing function closely. Consider alternative diuretics or antibiotics.", "Moderate",
  "Aspirin", "Cardiology", "Clopidogrel", "Cardiology", "Pharmacodynamic", "Additive antiplatelet effects.", "Increased risk of bleeding, especially GI bleeding.", "Often intentionally co-administered (dual antiplatelet therapy) post-PCI, but requires careful monitoring for bleeding.", "Moderate",
  "Metformin", "Non-Cardiology", "Iodinated Contrast Media", "Non-Cardiology", "Pharmacodynamic", "Increased risk of lactic acidosis.", "Acute kidney injury and lactic acidosis.", "Discontinue metformin at the time of or prior to iodinated contrast media administration in patients with eGFR <60 mL/min/1.73 m2, liver disease, alcoholism, or heart failure. Re-evaluate renal function 48 hours after procedure.", "Major",
  "Sildenafil", "Cardiology", "Nitroglycerin", "Cardiology", "Pharmacodynamic", "Potentiation of hypotensive effects.", "Severe, life-threatening hypotension.", "Absolute contraindication. Do not administer sildenafil within 24 hours of nitrate use.", "Contraindicated",
  "Amlodipine", "Cardiology", "Grapefruit Juice", "Non-Drug", "Pharmacokinetic (CYP3A4 inhibition)", "Grapefruit juice inhibits amlodipine metabolism, increasing drug levels.", "Increased risk of hypotension and peripheral edema.", "Advise patients to avoid grapefruit juice or consume in moderation. Monitor for adverse effects.", "Moderate",
  "Warfarin", "Cardiology", "Cranberry Juice", "Non-Drug", "Pharmacodynamic/Unknown", "May enhance anticoagulant effect of warfarin, increasing bleeding risk.", "Increased bleeding risk.", "Advise patients to avoid cranberry products or consume consistently. Monitor INR closely.", "Moderate",
  "Atorvastatin", "Cardiology", "Clarithromycin", "Non-Cardiology", "Pharmacokinetic (CYP3A4 inhibition)", "Clarithromycin significantly increases atorvastatin levels.", "Increased risk of myopathy and rhabdomyolysis.", "Avoid co-administration. If clarithromycin is needed, temporarily discontinue atorvastatin.", "Major",
  "Digoxin", "Cardiology", "Furosemide", "Cardiology", "Pharmacodynamic/Electrolyte", "Furosemide-induced hypokalemia can potentiate digoxin toxicity.", "Increased risk of digoxin toxicity.", "Monitor potassium levels and supplement if necessary. Monitor digoxin levels and clinical status.", "Moderate",
  "Aspirin", "Cardiology", "Lisinopril", "Cardiology", "Pharmacodynamic", "Aspirin (especially high dose) may reduce the antihypertensive effect of ACE inhibitors.", "Reduced blood pressure control.", "Monitor blood pressure. If high-dose aspirin is required, consider alternative antihypertensive agents or monitor BP closely.", "Minor",
  "Diltiazem", "Cardiology", "Simvastatin", "Cardiology", "Pharmacokinetic (CYP3A4 inhibition)", "Diltiazem increases simvastatin levels.", "Increased risk of myopathy.", "Limit simvastatin dose to 20 mg/day. Consider alternative statin.", "Moderate",
  "Amiodarone", "Cardiology", "Phenytoin", "Non-Cardiology", "Pharmacokinetic (CYP2C9/3A4 inhibition)", "Amiodarone increases phenytoin levels.", "Risk of phenytoin toxicity.", "Monitor phenytoin levels. Adjust dose as needed.", "Major",
  "Metoprolol", "Cardiology", "Clonidine", "Cardiology", "Pharmacodynamic", "Risk of rebound hypertension upon clonidine withdrawal.", "Severe hypertension if clonidine is abruptly stopped while on beta-blockers.", "Taper clonidine slowly. Discontinue beta-blocker several days before clonidine withdrawal.", "Major"
) %>%
  # Ensure all drug names are consistently ordered for easier matching (Drug1 always alphabetically before Drug2)
  rowwise() %>%
  mutate(
    DrugA = sort(c(Drug1, Drug2))[1],
    DrugB = sort(c(Drug1, Drug2))[2]
  ) %>%
  ungroup() %>%
  # CORRECTED: Ensure Drug1Class and Drug2Class are explicitly selected here
  select(DrugA, DrugB, Drug1Class, Drug2Class, InteractionType, Mechanism, ClinicalSignificance, Management, Severity)

# Get unique list of all drugs for selection
all_drugs <- unique(c(drug_interactions_data$DrugA, drug_interactions_data$DrugB))
all_drugs <- sort(all_drugs)

# Define drug classes for nodes (this is derived from the data, not explicitly needed here)
# drug_classes <- unique(c(drug_interactions_data$Drug1Class, drug_interactions_data$Drug2Class))

# --- Challenge Mode Data ---
challenge_cases <- list(
  list(
    id = 1,
    patient_scenario = "Mr. Smith, 68, has a history of hypertension and atrial fibrillation. He is currently on **Warfarin** and **Metoprolol**.",
    current_meds = c("Warfarin", "Metoprolol"),
    potential_additions = c("Amiodarone", "Ibuprofen", "Lisinopril", "Sildenafil", "Clarithromycin"),
    expected_interactions = list(
      c("Warfarin", "Amiodarone"),
      c("Warfarin", "Ibuprofen"),
      c("Metoprolol", "Sildenafil") # Sildenafil with beta-blocker can cause hypotension
    )
  ),
  list(
    id = 2,
    patient_scenario = "Ms. Jones, 75, presents with new onset heart failure. She is currently on **Digoxin** and **Furosemide**.",
    current_meds = c("Digoxin", "Furosemide"),
    potential_additions = c("Spironolactone", "Amiodarone", "Metformin", "Grapefruit Juice", "Lisinopril"),
    expected_interactions = list(
      c("Digoxin", "Amiodarone"),
      c("Digoxin", "Furosemide"), # Already on it, but interaction is relevant
      c("Furosemide", "Spironolactone")
    )
  ),
  list(
    id = 3,
    patient_scenario = "A 55-year-old male with hyperlipidemia is on **Simvastatin**. He develops a bacterial infection requiring antibiotics.",
    current_meds = c("Simvastatin"),
    potential_additions = c("Clarithromycin", "Amlodipine", "Gentamicin", "Cranberry Juice", "Lisinopril"),
    expected_interactions = list(
      c("Simvastatin", "Clarithromycin"),
      c("Simvastatin", "Amlodipine")
    )
  )
)


# --- User Interface (UI) ---
ui <- navbarPage(
  title = h1("Cardiology Drug Interaction Explorer", style = "color: #2c3e50; font-family: 'Inter', sans-serif; font-weight: 600; font-size: 2.2em;"),
  theme = shinytheme("flatly"),
  
  # Tab 1: Drug Explorer
  tabPanel(
    "Drug Explorer",
    icon = icon("magnifying-glass-chart"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;",
        h3("Select Medications", style = "color: #2c3e50; margin-bottom: 20px;"),
        
        selectizeInput(
          "explorer_selected_drugs",
          "Choose 2-5 drugs:",
          choices = all_drugs,
          multiple = TRUE,
          options = list(maxItems = 5, placeholder = "Start typing drug names...")
        ),
        
        hr(),
        p("This tool helps visualize potential drug-drug interactions. Select medications from the dropdown to see relevant information in the table and network graph.",
          style = "font-size: 0.9em; color: #7f8c8d; text-align: center;")
      ),
      
      mainPanel(
        width = 9,
        style = "background-color: #ffffff; padding: 30px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
        
        h3("Potential Drug Interactions", style = "color: #34495e;"),
        p("Interactions are identified based on the selected drugs. This is for educational purposes only and not a substitute for professional medical advice.",
          style = "font-size: 1.0em; line-height: 1.5; color: #555;"),
        hr(),
        
        conditionalPanel(
          condition = "input.explorer_selected_drugs.length < 2",
          div(class = "alert alert-warning", role = "alert",
              "Please select at least two drugs to check for interactions.")
        ),
        
        conditionalPanel(
          condition = "input.explorer_selected_drugs.length >= 2",
          h4("Interaction Network:", style = "color: #34495e;"),
          visNetworkOutput("interactions_network", height = "400px"),
          br(),
          h4("Interaction Details Table:", style = "color: #34495e;"),
          DTOutput("interactions_table")
        ),
        
        br(),
        h4("Common Interaction Types & Severity Explained:", style = "color: #34495e;"),
        fluidRow(
          column(6,
                 tags$ul(
                   tags$li(strong("Pharmacodynamic Interaction:"), " Drugs affect the body in similar or opposing ways, leading to additive or antagonistic effects (e.g., both cause bradycardia)."),
                   tags$li(strong("Pharmacokinetic Interaction:"), " One drug affects how the body handles another drug (absorption, distribution, metabolism, excretion). Often involves CYP450 enzymes or P-glycoprotein (P-gp).")
                 )
          ),
          column(6,
                 tags$ul(
                   tags$li(strong("Severity:"),
                           tags$span(style="color:green;", "Minor"), ", ",
                           tags$span(style="color:orange;", "Moderate"), ", ",
                           tags$span(style="color:red;", "Major"), ", ",
                           tags$span(style="color:darkred;", "Contraindicated")
                   ),
                   tags$li(strong("Node Colors:"),
                           tags$span(style="color:#3498db;", "Cardiology Drug"), ", ",
                           tags$span(style="color:#2ecc71;", "Non-Cardiology Drug"), ", ",
                           tags$span(style="color:#95a5a6;", "Non-Drug")
                   )
                 )
          )
        )
      )
    )
  ),
  
  # Tab 2: Clinical Challenge
  tabPanel(
    "Clinical Challenge",
    icon = icon("user-doctor"),
    fluidRow(
      column(4,
             style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;",
             h3("Patient Scenario", style = "color: #2c3e50;"),
             htmlOutput("challenge_scenario"),
             hr(),
             h4("Current Medications:", style = "color: #2c3e50;"),
             htmlOutput("challenge_current_meds"),
             hr(),
             h4("Additional Medications to Consider:", style = "color: #2c3e50;"),
             selectizeInput(
               "challenge_selected_additions",
               "Select drugs to prescribe:",
               choices = NULL, # Will be updated dynamically
               multiple = TRUE,
               options = list(placeholder = "Select drugs...")
             ),
             actionButton("submit_challenge", "Check Interactions", class = "btn-primary btn-block mt-3"),
             actionButton("next_challenge_case", "Next Case", class = "btn-secondary btn-block mt-2"),
             actionButton("reset_challenge", "Reset Challenge", class = "btn-danger btn-block mt-2")
      ),
      column(8,
             style = "background-color: #ffffff; padding: 30px; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
             h3("Challenge Feedback", style = "color: #34495e;"),
             hr(),
             h4("Your Score:", style = "color: #34495e;"),
             textOutput("challenge_score_display"),
             hr(),
             h4("Interactions Found:", style = "color: #34495e;"),
             DTOutput("challenge_feedback_table"),
             br(),
             h4("Explanation:", style = "color: #34495e;"),
             htmlOutput("challenge_explanation_text")
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Drug Explorer Tab Logic ---
  
  # Reactive expression to filter interactions based on selected drugs for Explorer tab
  explorer_filtered_interactions <- reactive({
    req(input$explorer_selected_drugs) # Ensure at least one drug is selected
    
    selected_count <- length(input$explorer_selected_drugs)
    if (selected_count < 2) {
      return(NULL) # Return NULL if less than 2 drugs are selected
    }
    
    # Generate all unique pairs from selected drugs, maintaining alphabetical order
    selected_pairs <- combn(sort(input$explorer_selected_drugs), 2, simplify = FALSE)
    
    # Convert pairs to a data frame for easier joining
    selected_pairs_df <- tibble(
      DrugA_selected = sapply(selected_pairs, `[`, 1),
      DrugB_selected = sapply(selected_pairs, `[`, 2)
    )
    
    # Filter the main interaction data
    # Ensure Drug1Class and Drug2Class are available for the network_data reactive
    interactions <- drug_interactions_data %>%
      inner_join(selected_pairs_df, by = c("DrugA" = "DrugA_selected", "DrugB" = "DrugB_selected")) %>%
      select(DrugA, DrugB, Drug1Class, Drug2Class, InteractionType, Mechanism, ClinicalSignificance, Management, Severity)
    
    if (nrow(interactions) == 0) {
      return(tibble(
        DrugA = character(), DrugB = character(), Drug1Class = character(), Drug2Class = character(),
        InteractionType = character(), Mechanism = character(), ClinicalSignificance = character(),
        Management = character(), Severity = character()
      )) # Return empty tibble with correct columns if no interactions
    }
    
    return(interactions)
  })
  
  # Render the interactive table of interactions for Explorer tab
  output$interactions_table <- renderDT({
    interactions_to_display <- explorer_filtered_interactions()
    
    if (is.null(interactions_to_display) || nrow(interactions_to_display) == 0) {
      datatable(
        data.frame(Message = "No known interactions found for the selected combination, or select more drugs."),
        options = list(dom = 't', paging = FALSE, searching = FALSE),
        rownames = FALSE
      )
    } else {
      datatable(
        interactions_to_display,
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 15, 20),
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '10%', targets = c(0, 1)),
            list(width = '15%', targets = 2),
            list(width = '20%', targets = 3),
            list(width = '20%', targets = 4),
            list(width = '20%', targets = 5),
            list(width = '5%', targets = 6)
          )
        ),
        rownames = FALSE,
        selection = 'single', # Allow single row selection for detail view
        class = 'display compact cell-border hover',
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #34495e; font-size: 1.2em;',
          'Identified Drug Interactions'
        )
      )
    }
  })
  
  # Reactive for network graph data
  network_data <- reactive({
    interactions_df <- explorer_filtered_interactions()
    
    if (is.null(interactions_df) || nrow(interactions_df) == 0) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }
    
    # Create nodes data frame
    # Ensure Drug1Class and Drug2Class are correctly used to define node groups/colors
    nodes_df <- interactions_df %>%
      select(id = DrugA, group = Drug1Class) %>% # Select DrugA and its class
      bind_rows(
        interactions_df %>%
          select(id = DrugB, group = Drug2Class) # Select DrugB and its class
      ) %>%
      distinct(id, .keep_all = TRUE) %>% # Keep only unique drugs
      mutate(
        label = id,
        color = case_when(
          group == "Cardiology" ~ "#3498db", # Blue
          group == "Non-Cardiology" ~ "#2ecc71", # Green
          TRUE ~ "#95a5a6" # Gray for Non-Drug
        ),
        title = paste0("Drug: ", id, "<br>Class: ", group) # Tooltip
      )
    
    # Create edges data frame
    edges_df <- interactions_df %>%
      mutate(
        from = DrugA,
        to = DrugB,
        label = InteractionType,
        title = paste0("Type: ", InteractionType, "<br>Mechanism: ", Mechanism, "<br>Significance: ", ClinicalSignificance, "<br>Management: ", Management),
        color = case_when(
          Severity == "Minor" ~ "green",
          Severity == "Moderate" ~ "orange",
          Severity == "Major" ~ "red",
          Severity == "Contraindicated" ~ "darkred",
          TRUE ~ "gray"
        ),
        width = case_when( # Thicker lines for more severe interactions
          Severity == "Minor" ~ 1,
          Severity == "Moderate" ~ 2,
          Severity == "Major" ~ 3,
          Severity == "Contraindicated" ~ 4,
          TRUE ~ 1
        )
      ) %>%
      select(from, to, label, title, color, width)
    
    list(nodes = nodes_df, edges = edges_df)
  })
  
  # Render the network graph
  output$interactions_network <- renderVisNetwork({
    net_data <- network_data()
    
    if (nrow(net_data$nodes) == 0) {
      visNetwork(nodes = data.frame(id = 1, label = "No drugs selected or no interactions found."), edges = data.frame()) %>%
        visOptions(highlightNearest = FALSE, nodesIdSelection = FALSE) %>%
        visLayout(randomSeed = 123)
    } else {
      visNetwork(nodes = net_data$nodes, edges = net_data$edges) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
          nodesIdSelection = TRUE, # Enable dropdown selector for nodes
          selectedBy = "id" # Select nodes by their ID
        ) %>%
        visEdges(smooth = list(enabled = TRUE, type = "continuous")) %>%
        visPhysics(stabilization = FALSE) %>% # Disable physics stabilization for faster rendering
        visLayout(randomSeed = 123) %>%
        visInteraction(navigationButtons = TRUE, zoomView = TRUE) # Add navigation buttons and zoom
    }
  })
  
  # --- Clinical Challenge Tab Logic ---
  
  challenge_current_case_index <- reactiveVal(1)
  challenge_score <- reactiveVal(0)
  
  # Update challenge scenario and current meds
  observe({
    current_case <- challenge_cases[[challenge_current_case_index()]]
    output$challenge_scenario <- renderText(current_case$patient_scenario)
    output$challenge_current_meds <- renderText({
      paste(current_case$current_meds, collapse = ", ")
    })
    
    # Update choices for additional medications based on current case
    updateSelectizeInput(
      session,
      "challenge_selected_additions",
      choices = current_case$potential_additions,
      selected = character(0), # Clear previous selection
      server = TRUE
    )
  })
  
  # Reactive to store feedback for the current challenge case
  challenge_feedback_reactive <- reactiveVal(NULL)
  
  # Observe submit button in challenge mode
  observeEvent(input$submit_challenge, {
    req(input$challenge_selected_additions)
    
    current_case <- challenge_cases[[challenge_current_case_index()]]
    patient_meds <- current_case$current_meds
    selected_additions <- input$challenge_selected_additions
    
    # Combine all drugs to check interactions
    all_drugs_in_scenario <- unique(c(patient_meds, selected_additions))
    
    # Generate all unique pairs from scenario drugs
    scenario_pairs <- combn(sort(all_drugs_in_scenario), 2, simplify = FALSE)
    
    # Convert pairs to a data frame for filtering
    scenario_pairs_df <- tibble(
      DrugA_scenario = sapply(scenario_pairs, `[`, 1),
      DrugB_scenario = sapply(scenario_pairs, `[`, 2)
    )
    
    # Find all actual interactions for this scenario
    actual_interactions <- drug_interactions_data %>%
      inner_join(scenario_pairs_df, by = c("DrugA" = "DrugA_scenario", "DrugB" = "DrugB_scenario"))
    
    # Filter for interactions involving selected additions
    relevant_actual_interactions <- actual_interactions %>%
      filter(DrugA %in% selected_additions | DrugB %in% selected_additions)
    
    # Determine expected interactions from the case data
    expected_interactions_formatted <- tibble(
      DrugA = sapply(current_case$expected_interactions, function(x) sort(x)[1]),
      DrugB = sapply(current_case$expected_interactions, function(x) sort(x)[2])
    )
    
    # --- Evaluate User's Answers ---
    identified_correctly <- tibble()
    missed_interactions <- tibble()
    false_positives <- tibble()
    
    score_change <- 0
    
    # Check for correctly identified interactions (True Positives)
    if (nrow(relevant_actual_interactions) > 0 && nrow(expected_interactions_formatted) > 0) {
      identified_correctly <- relevant_actual_interactions %>%
        inner_join(expected_interactions_formatted, by = c("DrugA", "DrugB")) %>%
        mutate(Status = "Correctly Identified")
      score_change <- score_change + nrow(identified_correctly) * 10 # Award points
    }
    
    # Check for missed interactions (False Negatives)
    if (nrow(expected_interactions_formatted) > 0) {
      missed_interactions <- expected_interactions_formatted %>%
        anti_join(relevant_actual_interactions, by = c("DrugA", "DrugB")) %>%
        left_join(drug_interactions_data, by = c("DrugA", "DrugB")) %>%
        mutate(Status = "Missed")
      score_change <- score_change - nrow(missed_interactions) * 5 # Deduct points
    }
    
    # Check for false positives (Identified but not actual interactions, or not expected for the case)
    if (nrow(relevant_actual_interactions) > 0) {
      false_positives <- relevant_actual_interactions %>%
        anti_join(expected_interactions_formatted, by = c("DrugA", "DrugB")) %>%
        mutate(Status = "False Positive")
      score_change <- score_change - nrow(false_positives) * 5 # Deduct points
    }
    
    # Combine all feedback
    feedback_df <- bind_rows(identified_correctly, missed_interactions, false_positives) %>%
      select(Status, DrugA, DrugB, InteractionType, Mechanism, ClinicalSignificance, Management, Severity)
    
    # Update score
    challenge_score(challenge_score() + score_change)
    
    challenge_feedback_reactive(feedback_df)
  })
  
  # Render feedback table for challenge mode
  output$challenge_feedback_table <- renderDT({
    feedback_df <- challenge_feedback_reactive()
    if (is.null(feedback_df) || nrow(feedback_df) == 0) {
      datatable(
        data.frame(Message = "Submit your answer to see feedback."),
        options = list(dom = 't', paging = FALSE, searching = FALSE),
        rownames = FALSE
      )
    } else {
      datatable(
        feedback_df,
        options = list(
          pageLength = 5,
          dom = 'tip',
          autoWidth = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(width = '10%', targets = 0), # Status
            list(width = '10%', targets = c(1, 2)), # Drugs
            list(width = '15%', targets = 3), # Type
            list(width = '20%', targets = 4), # Mechanism
            list(width = '20%', targets = 5), # Significance
            list(width = '20%', targets = 6), # Management
            list(width = '5%', targets = 7) # Severity
          )
        ),
        rownames = FALSE,
        selection = 'none',
        class = 'display compact cell-border hover'
      )
    }
  })
  
  # Display current score
  output$challenge_score_display <- renderText({
    paste0("Current Score: ", challenge_score())
  })
  
  # Next case button logic
  observeEvent(input$next_challenge_case, {
    if (challenge_current_case_index() < length(challenge_cases)) {
      challenge_current_case_index(challenge_current_case_index() + 1)
      challenge_feedback_reactive(NULL) # Clear feedback for new case
    } else {
      showModal(modalDialog(
        title = "Challenge Completed!",
        paste0("You've completed all cases! Your final score is: ", challenge_score()),
        footer = modalButton("Close")
      ))
    }
  })
  
  # Reset challenge button logic
  observeEvent(input$reset_challenge, {
    challenge_current_case_index(1)
    challenge_score(0)
    challenge_feedback_reactive(NULL)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
