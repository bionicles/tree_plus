# RTest.R
# S3 object
person <- list(name = "John Doe", age = 50)
class(person) <- "Person"

# S3 Method
greet.Person <- function(p) {
  paste("Hello,", p$name)
}

ensure_between = function(num, min_allowed, max_allowed) {
  max(min(num, max_allowed), min_allowed)
}

run_intermediate_annealing_process = function(cities, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                              starting_iteration, number_of_iterations,
                                              s_curve_amplitude, s_curve_center, s_curve_width) {
  n_cities = nrow(cities)
  
  for(i in 1:number_of_iterations) {
    iter = starting_iteration + i
    temp = current_temperature(iter, s_curve_amplitude, s_curve_center, s_curve_width)
    
    candidate_tour = tour
    swap = sample(n_cities, 2)
    candidate_tour[swap[1]:swap[2]] = rev(candidate_tour[swap[1]:swap[2]])
    candidate_dist = calculate_tour_distance(candidate_tour, distance_matrix)

    if (temp > 0) {
      ratio = exp((tour_distance - candidate_dist) / temp)
    } else {
      ratio = as.numeric(candidate_dist < tour_distance)
    }
    
    if (runif(1) < ratio) {
      tour = candidate_tour
      tour_distance = candidate_dist
      
      if (tour_distance < best_distance) {
        best_tour = tour
        best_distance = tour_distance
      }
    }
  }
  
  return(list(tour=tour, tour_distance=tour_distance, best_tour=best_tour, best_distance=best_distance))
}