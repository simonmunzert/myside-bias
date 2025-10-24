generate_filter_block <- function(data, filter_vars, message_vars, display_names) {
  library(jsonlite)
  library(purrr)
  library(htmltools)
  library(dplyr)
  
  ns_prefix <- "filter_"
  
  # Safety check: Clean whitespace and ensure character class
  data <- data %>%
    mutate(across(all_of(filter_vars), ~ trimws(as.character(.)))) %>%
    filter(across(all_of(filter_vars), ~ !is.na(.) & . != ""))
  
  # Validate columns strictly
  missing_cols <- setdiff(c(filter_vars, message_vars), names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Get defaults from first valid row
  defaults <- data[1, ]
  
  # Convert data to JSON for JS
  data_json <- toJSON(data, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
  
  # Build UI elements safely using purrr
  ui_elements <- map_chr(filter_vars, function(var) {
    values <- unique(data[[var]])
    label <- display_names[[var]]
    ns_var <- paste0(ns_prefix, var)
    
    if (length(values) <= 4) {
      radios <- map_chr(values, function(val) {
        checked <- ifelse(val == defaults[[var]], "checked", "")
        sprintf('<div class="form-check form-check-inline">
                  <input class="form-check-input" type="radio" name="%s" value="%s" %s>
                  <label class="form-check-label">%s</label>
                </div>', ns_var, val, checked, val)
      }) %>% paste(collapse = "\n")
      sprintf('<div class="col-md-4"><label class="form-label">%s:</label><br>%s</div>', label, radios)
    } else {
      options <- map_chr(values, function(val) {
        selected <- ifelse(val == defaults[[var]], "selected", "")
        sprintf('<option value="%s" %s>%s</option>', val, selected, val)
      }) %>% paste(collapse = "\n")
      sprintf('<div class="col-md-4"><label for="%s" class="form-label">%s:</label><select id="%s" name="%s" class="form-select">%s</select></div>', ns_var, label, ns_var, ns_var, options)
    }
  })
  
  ui_block <- paste(ui_elements, collapse = "\n")
  
  # Build block with namespaced, simplified and robust event handling
  html_block <- sprintf('
<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css" rel="stylesheet">

<div class="card mt-4">
  <div class="card-body">
    <h5 class="card-title"><b>Message selection</b></h5>
    <div class="row g-3 align-items-center">
      %s
    </div>
  </div>
</div>

<div class="card mt-3">
  <div class="card-body">
    <h5 class="card-title">Resulting messages</h5>
    <div id="filter_result" style="background-color: rgba(255,0,0,0.1); padding: 1rem; border-radius: 0.25rem; font-weight:bold; color:#333;">Please adjust the filters.</div>
  </div>
</div>

<script>

document.addEventListener("DOMContentLoaded", function() {

  var data = %s;

/* Attach change event only to this forms inputs */
                        document.querySelectorAll("select[name^=\'filter_\'], input[type=radio][name^=\'filter_\']").forEach(function(el) {
                          el.addEventListener("change", filterData);
                        });
                        
                        function filterData() {
                          var criteria = {};
                          
                          // Gather select inputs
                          document.querySelectorAll("select[name^=\'filter_\']").forEach(function(el) {
                            criteria[el.name.replace("filter_", "")] = el.value;
                          });
                          
                          // Gather radio buttons by name
                          document.querySelectorAll("input[type=radio][name^=\'filter_\']").forEach(function(el) {
                            if (!criteria[el.name.replace("filter_", "")]) {
                              var checked = document.querySelector("input[name=" + el.name + "]:checked");
                              if (checked) {
                                criteria[el.name.replace("filter_", "")] = checked.value;
                              }
                            }
                          });
                          
                          console.log("Selected criteria:", criteria);
                          
                          var filtered = data.filter(function(row) {
                            return Object.keys(criteria).every(function(key) {
                              return String(row[key]) === String(criteria[key]);
                            });
                          });
                          
                          console.log("Filtered matches:", filtered.length);
                          
                          var resultDiv = document.getElementById("filter_result");
                          
                          if (filtered.length === 0) {
                            resultDiv.innerHTML = "No matching data found.";
                          } else {
                            var row = filtered[0];
                            resultDiv.innerHTML = "<b>Target message:</b><br> " + row.%s + "<br><br><b>Sender message:</b><br> " + row.%s;
                          }
                        }
                        
                        filterData();
                        
});
  
  </script>
    ',
ui_block,
data_json,
message_vars[1],
message_vars[2]
  )
  
  return(htmltools::HTML(html_block))
}
