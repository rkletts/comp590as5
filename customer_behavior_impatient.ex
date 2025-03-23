defmodule CustomerBehavior do
  def wait_for_haircut(barber, id) do
    wait_time_limit = :rand.uniform(5000) + 5000


    receive do
      :haircut_done ->
        IO.puts("Customer #{id} got haircut and leaves happy")
        exit(:normal)
    after
      wait_time_limit ->
        IO.puts("Customer #{id} got frustrated and leaves after waiting too long")
        exit(:normal)
    end
  end
end
