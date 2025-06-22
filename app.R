# app.R

# Load necessary libraries
library(shiny)
library(dplyr) # Not strictly necessary for this simple app, but good practice for data work

# --- FLAMES Calculation Function ---
# This function calculates the FLAMES relationship between two names.
flames_calculate <- function(name1_val, name2_val) {
  # Convert names to lowercase and remove spaces for consistent comparison
  name1 <- tolower(gsub(" ", "", name1_val))
  name2 <- tolower(gsub(" ", "", name2_val))
  
  if (nchar(name1) == 0 || nchar(name2) == 0) {
    return("Please enter both names to calculate FLAMES.")
  }
  
  # Create frequency maps for each name
  freq1 <- table(strsplit(name1, "")[[1]])
  freq2 <- table(strsplit(name2, "")[[1]])
  
  # Calculate remaining characters by finding the absolute difference in counts for each character
  remaining_count <- 0
  
  # Get all unique characters present in both names combined
  all_chars <- unique(c(names(freq1), names(freq2)))
  
  for (char in all_chars) {
    # Get count of character in name1, default to 0 if not present
    count1 <- as.numeric(freq1[char])
    if (is.na(count1)) count1 <- 0
    
    # Get count of character in name2, default to 0 if not present
    count2 <- as.numeric(freq2[char])
    if (is.na(count2)) count2 <- 0
    
    # The number of non-common characters for this specific character
    remaining_count <- remaining_count + abs(count1 - count2)
  }
  
  # Handle cases where remaining_count might be 0 (e.g., identical names)
  if (remaining_count == 0) {
    return("The names have many common letters! Try different names for a clearer FLAMES result.")
  }
  
  # Define the FLAMES word and its meanings
  flames_word <- c("F", "L", "A", "M", "E", "S")
  flames_meaning <- c(
    "Friends",
    "Lovers",
    "Affectionate",
    "Marriage",
    "Enemies",
    "Siblings"
  )
  
  # Calculate the index within the FLAMES word using modulo
  index <- (remaining_count %% length(flames_word))
  if (index == 0) {
    index <- length(flames_word) # If remainder is 0, it's the last element
  }
  
  result_letter <- flames_word[index]
  result_meaning <- flames_meaning[index]
  
  # Format the result for display
  return(paste0("The FLAMES result for ", name1_val, " and ", name2_val, " is: <br><span style='font-size:1.5em; font-weight:bold; color:#B22222;'>", result_meaning, " (", result_letter, ")</span>"))
}


# --- User Interface (UI) ---
ui <- fluidPage(
  # Set a warm and inviting background color with flame-like tones
  tags$head(
    tags$style(HTML("
      body {
        background-color: #FFF8DC; /* Cornsilk - a warm, light yellow */
        font-family: 'Georgia', serif;
        color: #4A2E0C; /* Dark brown for text */
      }
      .title-panel {
        background-color: #B22222; /* Firebrick - a deep, passionate red */
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        text-align: center;
        color: #FFD700; /* Gold for contrast */
        box-shadow: 0 6px 12px rgba(0,0,0,0.2);
        animation: glow 1.5s infinite alternate; /* Subtle glow effect */
      }
      @keyframes glow {
        from { box-shadow: 0 6px 12px rgba(0,0,0,0.2); }
        to { box-shadow: 0 6px 20px rgba(255,165,0,0.4); } /* Orange glow */
      }
      .well {
        background-color: #FFFAF0; /* Floral White - soft white for input panel */
        border-radius: 10px;
        box-shadow: 0 3px 6px rgba(0,0,0,0.15);
      }
      .flames-output { /* Apply styles to output */
        background-color: #FFFFFF;
        padding: 30px;
        border-radius: 15px;
        border: 2px solid #FFA500; /* Orange border */
        box-shadow: 0 8px 16px rgba(0,0,0,0.2);
        font-size: 1.3em;
        line-height: 1.6;
        text-align: center;
        margin-top: 20px;
        color: #8B4513; /* SaddleBrown for greeting text */
        animation: pulse 2s infinite alternate; /* Subtle pulse effect for greeting */
      }
      @keyframes pulse {
        from { transform: scale(1); }
        to { transform: scale(1.01); }
      }
      .btn-primary {
        background-color: #FF4500; /* OrangeRed - a vibrant, fiery red-orange */
        border-color: #FF4500;
        color: white;
        font-weight: bold;
        padding: 12px 25px;
        border-radius: 10px;
        font-size: 1.1em;
        transition: background-color 0.3s ease, transform 0.2s ease;
        margin-right: 10px; /* Space between buttons */
      }
      .btn-primary:hover {
        background-color: #CD5C5C; /* Indian Red */
        border-color: #CD5C5C;
        transform: translateY(-2px);
      }
      .btn-default { /* Style for the reset button */
        background-color: #D3D3D3; /* Light gray */
        border-color: #D3D3D3;
        color: #333;
        font-weight: bold;
        padding: 12px 25px;
        border-radius: 10px;
        font-size: 1.1em;
        transition: background-color 0.3s ease, transform 0.2s ease;
      }
      .btn-default:hover {
        background-color: #A9A9A9; /* Darker gray */
        border-color: #A9A9A9;
        transform: translateY(-2px);
      }
      .explanation-text {
        background-color: #F8F8F8;
        padding: 25px;
        border-radius: 10px;
        border: 1px solid #E0E0E0;
        margin-top: 30px;
        font-size: 0.95em;
        line-height: 1.5;
        color: #555;
      }
      .explanation-text h4 {
        color: #B22222;
        font-size: 1.4em;
        margin-bottom: 15px;
      }
      .flames-list {
        list-style-type: none; /* Remove default bullet points */
        padding-left: 0;
        text-align: left;
        display: inline-block; /* Keep list centered */
      }
      .flames-list li {
        margin-bottom: 5px;
      }
      .flames-list li strong {
        color: #FF4500; /* OrangeRed for FLAMES letters */
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("FLAMES Calculator") # Updated title for only FLAMES
  ),
  
  # Directly use sidebarLayout for the single functionality
  sidebarLayout(
    sidebarPanel(
      textInput("flamesName1", "Name 1", value = "John"), # Adjusted label
      textInput("flamesName2", "Name 2", value = "Jane"), # Adjusted label
      fluidRow( # Use fluidRow to place buttons side-by-side
        column(6, actionButton("calculateFlames", "Calculate", icon = icon("calculator"), class = "btn-primary")),
        column(6, actionButton("resetFlames", "Reset", icon = icon("sync-alt"), class = "btn-default")) # Added Reset button
      )
    ),
    mainPanel(
      h3("FLAMES Result:"),
      div(class = "flames-output",
          htmlOutput("flamesResult") # Output for FLAMES result
      ),
      
      hr(), # Horizontal rule for separation
      
      div(class = "explanation-text",
          h4("How does this FLAMES calculator work?"),
          p("This FLAMES calculator assesses and predicts the outcome of a relationship based on a simple algorithm of two given names. It tries to find the answer to questions like 'What is our relationship?' or to give you a sense of what is going on between you and another person."),
          p("The calculation involves removing common letters from both names and then counting the remaining unique letters. This count determines which letter of the word FLAMES corresponds to your relationship status. FLAMES stands for:"),
          tags$ul(class = "flames-list",
                  tags$li(tags$strong("F:"), " Friends"),
                  tags$li(tags$strong("L:"), " Lovers"),
                  tags$li(tags$strong("A:"), " Affectionate"),
                  tags$li(tags$strong("M:"), " Marriage"),
                  tags$li(tags$strong("E:"), " Enemies"),
                  tags$li(tags$strong("S:"), " Siblings")
          ),
          p("While it's a fun game, remember that true relationships are complex and cannot be predicted by a simple algorithm!")
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive value to store the FLAMES result
  flames_result_text <- eventReactive(input$calculateFlames, {
    # Pass input values as arguments to the function
    flames_calculate(input$flamesName1, input$flamesName2)
  })
  
  # Render the FLAMES result
  output$flamesResult <- renderUI({
    HTML(flames_result_text())
  })
  
  # Observer for the Reset button
  observeEvent(input$resetFlames, {
    updateTextInput(session, "flamesName1", value = "") # Clear Name 1 input
    updateTextInput(session, "flamesName2", value = "") # Clear Name 2 input
    output$flamesResult <- renderUI({ HTML("") }) # Clear the result display
  })
}

# Run the application
shinyApp(ui = ui, server = server)
