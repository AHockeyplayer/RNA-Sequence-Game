# RNA-Sequence-Game
An RNA themed interactive game about DNA transcription and RNA translation built in R.
# DNA to RNA transcription function
transcribe_dna_to_rna <- function(dna_sequence) {
  rna_sequence <- gsub("T", "U", dna_sequence)
  return(rna_sequence)
}

# RNA Codon to Amino Acid table
codon_table <- list(
  "UUU" = "Phe", "UUC" = "Phe", "UUA" = "Leu", "UUG" = "Leu",
  "CUU" = "Leu", "CUC" = "Leu", "CUA" = "Leu", "CUG" = "Leu",
  "AUU" = "Ile", "AUC" = "Ile", "AUA" = "Ile", "AUG" = "Met",
  "GUU" = "Val", "GUC" = "Val", "GUA" = "Val", "GUG" = "Val",
  "UCU" = "Ser", "UCC" = "Ser", "UCA" = "Ser", "UCG" = "Ser",
  "CCU" = "Pro", "CCC" = "Pro", "CCA" = "Pro", "CCG" = "Pro",
  "ACU" = "Thr", "ACC" = "Thr", "ACA" = "Thr", "ACG" = "Thr",
  "GCU" = "Ala", "GCC" = "Ala", "GCA" = "Ala", "GCG" = "Ala",
  "UAU" = "Tyr", "UAC" = "Tyr", "UAA" = "STOP", "UAG" = "STOP",
  "CAU" = "His", "CAC" = "His", "CAA" = "Gln", "CAG" = "Gln",
  "AAU" = "Asn", "AAC" = "Asn", "AAA" = "Lys", "AAG" = "Lys",
  "GAU" = "Asp", "GAC" = "Asp", "GAA" = "Glu", "GAG" = "Glu",
  "UGU" = "Cys", "UGC" = "Cys", "UGA" = "STOP", "UGG" = "Trp",
  "CGU" = "Arg", "CGC" = "Arg", "CGA" = "Arg", "CGG" = "Arg",
  "AGU" = "Ser", "AGC" = "Ser", "AGA" = "Arg", "AGG" = "Arg",
  "GGU" = "Gly", "GGC" = "Gly", "GGA" = "Gly", "GGG" = "Gly"
)

# Function to transcribe RNA sequence into protein sequence
transcribe_rna_to_protein <- function(rna_sequence) {
  codons <- unlist(strsplit(rna_sequence, "(?<=...)", perl = TRUE))
  protein <- sapply(codons, function(codon) {
    if (codon %in% names(codon_table)) {
      return(codon_table[[codon]])
    } else {
      return("Invalid Codon")
    }
  })
  return(paste(protein, collapse = "-"))
}

# Generate random DNA sequence
generate_dna_sequence <- function(length) {
  bases <- c("A", "T", "C", "G")
  sequence <- sample(bases, length, replace = TRUE)
  return(paste(sequence, collapse = ""))
}

# Define the UI for the application
ui <- fluidPage(
  
  # Include some styling
  tags$head(tags$style(
    HTML("
      h3 {color: #2c3e50;}
      body {background-color: #f4f4f4;}
      .well {background-color: #ffffff;}
      .dragItem {cursor: move; padding: 8px; margin: 5px; background-color: #3498db; color: white; font-weight: bold;}
    ")
  )),
  
  titlePanel("RNA Mastermind Game"),
  
  fluidRow(
    column(4,
      h3("Game Controls"),
      actionButton("start_game", "Start Game"),
      br(), br(),
      h4("Score"),
      textOutput("score_output")
    ),
    
    column(8,
      h3("Game Instructions"),
      
      wellPanel(
        h4("Challenge 1: Transcribe DNA to RNA"),
        textOutput("dna_seq"),
        jqui_sortable(div(id = "rna_sequence")),
        p("Drag the RNA bases below to build the RNA sequence:"),
        jqui_draggable(div(class = "dragItem", "A")),
        jqui_draggable(div(class = "dragItem", "U")),
        jqui_draggable(div(class = "dragItem", "C")),
        jqui_draggable(div(class = "dragItem", "G")),
        actionButton("submit_rna", "Submit RNA Sequence")
      ),
      
      wellPanel(
        h4("Challenge 2: Translate RNA to Protein"),
        textOutput("rna_seq"),
        jqui_sortable(div(id = "protein_sequence")),
        p("Drag the amino acids below to build the protein sequence:"),
        jqui_draggable(div(class = "dragItem", "Met")),
        jqui_draggable(div(class = "dragItem", "Leu")),
        jqui_draggable(div(class = "dragItem", "Ser")),
        jqui_draggable(div(class = "dragItem", "STOP")),
        jqui_draggable(div(class = "dragItem", "Gly")),
        jqui_draggable(div(class = "dragItem", "Arg")),
        actionButton("submit_protein", "Submit Protein Sequence")
      )
    )
  )
)

# Define server logic for the game
server <- function(input, output, session) {
  
  # Reactive values to store game state
  game_state <- reactiveValues(
    dna_seq = NULL,
    rna_seq = NULL,
    protein_seq = NULL,
    score = 0
  )
  
  # Start the game by generating a new DNA sequence
  observeEvent(input$start_game, {
    game_state$dna_seq <- generate_dna_sequence(15)
    game_state$rna_seq <- transcribe_dna_to_rna(game_state$dna_seq)
    game_state$protein_seq <- transcribe_rna_to_protein(game_state$rna_seq)
    game_state$score <- 0
    
    output$dna_seq <- renderText({
      paste("DNA Sequence:", game_state$dna_seq)
    })
    
    output$score_output <- renderText({
      paste("Score:", game_state$score)
    })
  })
  
  # Submit RNA sequence and check the answer
  observeEvent(input$submit_rna, {
    player_rna <- paste(input$rna_sequence, collapse = "")
    if (player_rna == game_state$rna_seq) {
      game_state$score <- game_state$score + 10
      showNotification("Correct RNA transcription!", type = "message")
    } else {
      showNotification("Incorrect RNA transcription.", type = "error")
    }
    
    output$score_output <- renderText({
      paste("Score:", game_state$score)
    })
    
    output$rna_seq <- renderText({
      paste("RNA Sequence:", game_state$rna_seq)
    })
  })
  
  # Submit Protein sequence and check the answer
  observeEvent(input$submit_protein, {
    player_protein <- paste(input$protein_sequence, collapse = "-")
    if (player_protein == game_state$protein_seq) {
      game_state$score <- game_state$score + 15
      showNotification("Correct Protein translation!", type = "message")
    } else {
      showNotification("Incorrect Protein translation.", type = "error")
    }
    
    output$score_output <- renderText({
      paste("Score:", game_state$score)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
