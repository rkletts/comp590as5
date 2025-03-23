defmodule BarberBehavior do
  def cut_hair do
    time = :rand.uniform(4000) + 2000
    IO.puts("Cutting hair for #{time}ms")
    Process.sleep(time)
  end
end
