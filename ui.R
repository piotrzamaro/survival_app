library(shinydashboard)
library(IPDfromKM)
library(survminer)
library(plotly)
library(ggplot2)
library(shiny)
library(dplyr)
library(survRM2)


ui <- dashboardPage(
  dashboardHeader(title = "Aplikacja Survival KM"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analiza na podstawie rekonstrukcji", tabName = "analiza"),
      menuItem("Analiza na podstawie danych RWE", tabName = "analiza1"),
      menuItem("Metodyka", tabName = "metodyka"),
      menuItem("Literatura", tabName = "literatura")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "analiza",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Wprowadź dane",
                    helpText("Wczytaj współrzędne przebiegu krzywych Kaplana-Meiera w postaci dwuloumnowego zestawu danych zawierających kolumny x i y"),
                    fileInput("file1", "Grupa interwencyjna", 
                              accept = ".xlsx"),
                    fileInput("file2", "Grupa kontrolna", 
                              accept = ".xlsx"),
                    helpText("Wybierz skalę osi y zgodną z wykresem źródłowym"),
                    selectInput("maxy", label = "Skala osi y", choices = c(1,100), selected = 1),
                    helpText("Ustaw horyzont badania odpowiadający skali x"),
                    sliderInput("maxx", "Horyzont badania (oś y)", min = 0, max = 150, value = 35),
                    helpText("Wpisz interwał czasu badania odpowiadający przedziałom skali x horyzont badania odpowiadający przedziałom skali x"),
                    numericInput("intervalx", "Interwał‚ osi y", value = 5),
                    helpText("Na podstawie danych tabeli 'at risk' wpisz liczbę pacjentów narażonych na wystąpienie zdarzenia odpowiadającą interwałom osi y"),
                    textInput("nrisk_arm0", "Pacjenci w zagrożeniu (interwencja)", "78,45,30,20,11,5,1,0"),
                    textInput("nrisk_arm1", "Pacjenci w zagrożeniu (komparator)", "38,14,6,2,2,0,0,0"),
                    helpText("Wpisz etykiety identyfikujące ramiona badania"),
                    textInput("label_arm0", "Etykieta grupy interwencyjnej", "interwencja"),
                    textInput("label_arm1", "Etykieta grupy kontrolnej", value = "komparator")
                ),
                box(title = "Zrekonstruowane dane IPD",
                    tableOutput("ipd")),
                box(solidHeader = T,
                    tableOutput("km_table")),
                box(title = "Krzywe przeżycia Kaplana-Meiera",
                    plotOutput("km_plot", height = 250)),
                box(title = "RMST",
                    plotOutput("rm_plot")),
              )),
      tabItem(tabName = "analiza1",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = "Wprowadź dane RWE",
                    helpText("Wprowadź dane RWE zawierające kulomny: time, status oraz arm"),
                    fileInput("file3", "Dane RWE", 
                              accept = ".xlsx")),
                box(title = "Dane RWE",
                        tableOutput("rwe")),
                box(solidHeader = T,
                        tableOutput("rwe_km_table")),
                box(title = "Krzywe przeżycia Kaplana-Meiera",
                        plotOutput("rwe_km_plot", height = 250)),
                box(title = "RMST",
                        plotOutput("rwe_rm_plot")))),
      tabItem(tabName = "metodyka",
                      fluidRow(
                        box(title = "Metodyka",
                            h4("Niniejsza aplikacja została zbudowana przy użyciu pakietu Shiny (wersja 1.7.1) w środowisku R (wersja 4.0.3) przy użyciu programu RStudio (wersja 1.3.959). 
W analizie odtworzono przebieg krzywych Kaplana-Meiera reprezentujących prawdopodobieństwo przeżycia wraz przedziałami ufności (95% CI). Wielkość efektu interwencji oceniono obliczając obszar pod przebiegiem odtworzonych krzywych Kaplana-Meiera uzyskując medianowy czas przeżycia oraz ograniczony średni czas przeżycia (ang. restricted mean survival time, RMST). 
"),
                            h3("Rekonstrukcja danych"),
                            h4("W procesie analitycznym zastosowano procedurę rekonstrukcji danych do poziomu pacjenta. Procedurę odczytu i rekonstrukcji indywidualnych danych pacjenta (ang. IPD - indyvidual patient data) przeprowadzono przy użyciu pakietu IPDfromKM  (wersja 	0.1.10). Jest to metoda odtworzenia pierwotnych danych na podstawie opublikowanych krzywych przeżycia Kaplana-Meiera. Uzyskane dane IPD posłużyły do oszacowania medianowego czasu przeżycia oraz ograniczonego średniego czasu przeżycia. 
Oszacowano wartości RMST horyzoncie badania (punkt odcięcia tau równy maksymalnemu wspólnemu punktowi czasowemu dla obydwu ramion badania. Ograniczony średni czas przeżycia oszacowano korzystając z pakietu survRM2 (wersja 1.0-3) . Uzyskane wartości przedstawiają obszar pod przebiegiem krzywych Kaplana-Meiera.
"),
                            h5("Źródło: Na Liu and J.Jack Lee (2020). IPDfromKM: Map Digitized Survival Curves Back to Individual Patient Data. R package version 0.1.10.  https://CRAN.R-project.org/package=IPDfromKM"),
                            h3("RMST"),
                            h4("W procesie analitycznym zastosowano procedurę rekonstrukcji danych do poziomu pacjenta. Procedurę odczytu i rekonstrukcji indywidualnych danych pacjenta (ang. IPD - indyvidual patient data) przeprowadzono przy użyciu pakietu IPDfromKM  (wersja 	0.1.10). Jest to metoda odtworzenia pierwotnych danych na podstawie opublikowanych krzywych przeżycia Kaplana-Meiera. Uzyskane dane IPD posłużyły do oszacowania medianowego czasu przeżycia oraz ograniczonego średniego czasu przeżycia. 
Oszacowano wartości RMST horyzoncie badania (punkt odcięcia tau równy maksymalnemu wspólnemu punktowi czasowemu dla obydwu ramion badania. Ograniczony średni czas przeżycia oszacowano korzystając z pakietu survRM2 (wersja 1.0-3) . Uzyskane wartości przedstawiają obszar pod przebiegiem krzywych Kaplana-Meiera.
"), 
                            h5("Źródło: Hajime Uno, Lu Tian, Miki Horiguchi, Angel Cronin, Chakib Battioui and James Bell (2020). survRM2: Comparing Restricted Mean Survival Time. R package version 1.0-3. https://CRAN.R-project.org/package=survRM0032"),
                            h3("Medianowy czas przeżycia"),
                            h4("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat"),
                            h5("Źródło: ")))))))

