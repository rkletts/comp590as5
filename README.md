# COMP590: Assignment 5: Elixir programming

# Team Members: Maria Thomas and Reese Letts

# README

## Design Rationale

For this project, we implemented a simulation of the barbershop problem using Elixir processes. The goal was to model customer behavior and barber behavior, experimenting with different conditions like impatient customers and slow barbers to see how they interact.

### Design Decisions

1. Process Communication: We used message passing between processes to simulate how customers and the barber interact. The customer waits for a haircut by listening for a :haircut_done message, and the barber sends that message when they finish cutting hair.

2. Customer Variants: We created two versions of the customer:
   - customer_behavior_original.ex represents a patient customer who waits indefinitely for a haircut.
   - customer_behavior_impatient.ex adds a timeout mechanism, where the customer gives up and leaves if the barber takes too long. This was done using the after clause in receive, adding randomness to simulate unpredictability.

3. Barber Variants: The barber also has two versions:
   - barber_behavior_original.ex cuts hair for a random time between 2 to 6 seconds.
   - barber_behavior_slow.ex takes a fixed 15 seconds per haircut to simulate a slower barber. This usually causes the waiting room to become full quicker and more customers getting turned away as a result.

4. Randomness for Realism: We used random wait times for both customers and barbers to make the simulation feel more natural. The impatient customer’s tolerance is randomly set between 5 to 10 seconds, and the barber’s haircut time varies in the original version.

5. Exit Conditions: Once a customer receives a haircut or leaves due to impatience, they call exit(:normal), which stops their process. This keeps the system from accumulating unnecessary processes over time.

### Overall Design

The overall design is simple but effective. By separating customer and barber behaviors into different modules, we were able to modify, run, and test different scenarios. The use of Elixir’s message-passing concurrency made it easy to simulate real waiting and processing times. 

### To Test Hotswap for Customers:
After starting the program in iex with c("sleeping_barber.exs"), do c("customer_behavior_impatient.ex"). To revert back to original behavior, do c("customer_behavior_original.ex")

### To Test Hotswap for Barber:
After starting the program in iex with c("sleeping_barber.exs"), do c("barber_behavior_slow.ex"). To revert back to original behavior, do c("barber_behavior_original.ex")

