defmodule BarberBehavior do
    def cut_hair do
        time = 15_000
        IO.puts("Cutting hair for #{time}ms (taking extra time for a fancy haircut)")
        Process.sleep(time)
    end
end
