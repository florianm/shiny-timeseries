library(shiny)
shinyUI(fluidPage(
    title = 'Timeseries explorer',
    titlePanel("Timeseries explorer"),
    fluidRow(
        column(3,
               wellPanel(
                   h4("Filter"),
                   sliderInput("reviews", "Minimum number of reviews on Rotten Tomatoes",
                               10, 300, 80, step = 10),
                   sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014)),
                   sliderInput("oscars", "Minimum number of Oscar wins (all categories)",
                               0, 4, 0, step = 1),
                   sliderInput("boxoffice", "Dollars at Box Office (millions)",
                               0, 800, c(0, 800), step = 1),
                   selectInput("genre", "Genre (a movie can have multiple genres)",
                               c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
                                 "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                                 "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                 "Short", "Sport", "Thriller", "War", "Western")
                   ),
                   textInput("director", "Director name contains (e.g., Miyazaki)"),
                   textInput("cast", "Cast names contains (e.g. Tom Hanks)")
               ),
               wellPanel(
                   selectInput("xvar", "X-axis variable", axis_vars, selected = "Meter"),
                   selectInput("yvar", "Y-axis variable", axis_vars, selected = "Reviews"),
                   tags$small(paste0(
                       "Note: The Tomato Meter is the proportion of positive reviews",
                       " (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
                       " a normalized 1-10 score of those reviews which have star ratings",
                       " (for example, 3 out of 4 stars)."
                   ))
               ),
               wellPanel(
                   selectInput(
                       'e0', '0. An ordinary select input', choices = state.name,
                       selectize = FALSE
                   ),
                   selectizeInput(
                       'e1', '1. A basic example (zero-configuration)',
                       choices = state.name
                   ),
                   selectizeInput(
                       'e2', '2. Multi-select', choices = state.name, multiple = TRUE
                   ),
                   selectizeInput(
                       'e3', '3. Item creation', choices = state.name,
                       options = list(create = TRUE)
                   ),
                   selectizeInput(
                       'e4', '4. Max number of options to show', choices = state.name,
                       options = list(maxOptions = 5)
                   ),
                   selectizeInput(
                       'e5', '5. Max number of items to select', choices = state.name,
                       multiple = TRUE, options = list(maxItems = 2)
                   ),
                   # I() indicates it is raw JavaScript code that should be evaluated, instead
                   # of a normal character string
                   selectizeInput(
                       'e6', '6. Placeholder', choices = state.name,
                       options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue(""); }')
                       )
                   ),
                   selectInput(
                       'e7', '7. selectInput() does not respond to empty strings',
                       choices = state.name
                   )
               )
        ),
        column(9,
               helpText('Output of the examples in the left:'),
               verbatimTextOutput('ex_out'),
               # use Github instead
               selectizeInput('github', 'Select a Github repo', choices = '', options = list(
                   valueField = 'url',
                   labelField = 'name',
                   searchField = 'name',
                   options = list(),
                   create = FALSE,
                   render = I("{
                              option: function(item, escape) {
                              return '<div>' +
                              '<strong><img src=\"http://brianreavis.github.io/selectize.js/images/repo-' + (item.fork ? 'forked' : 'source') + '.png\" width=20 />' + escape(item.name) + '</strong>:' +
                              ' <em>' + escape(item.description) + '</em>' +
                              ' (by ' + escape(item.username) + ')' +
                              '<ul>' +
                              (item.language ? '<li>' + escape(item.language) + '</li>' : '') +
                              '<li><span>' + escape(item.watchers) + '</span> watchers</li>' +
                              '<li><span>' + escape(item.forks) + '</span> forks</li>' +
                              '</ul>' +
                              '</div>';
                              }
                              }"),
        score = I("function(search) {
                  var score = this.getScoreFunction(search);
                  return function(item) {
                  return score(item) * (1 + Math.min(item.watchers / 100, 1));
                  };
        }"),
        load = I("function(query, callback) {
                 if (!query.length) return callback();
                 $.ajax({
                 url: 'https://api.github.com/legacy/repos/search/' + encodeURIComponent(query),
                 type: 'GET',
                 error: function() {
                 callback();
                 },
                 success: function(res) {
                 callback(res.repositories.slice(0, 10));
                 }
                 });
        }")
      )),
      helpText('If the above searching fails, it is probably the Github API limit
                 has been reached (5 per minute). You can try later.'),
      verbatimTextOutput('github')
        )
    )
))

