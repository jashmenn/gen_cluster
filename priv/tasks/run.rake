def root
  File.dirname(__FILE__)  + "/../../"
end

SERVER   = "example_cluster_srv1@localhost"

namespace :server do
  1.upto(4) do |i| 
    desc "start server #{i}"
    task "start#{i}" => [:compile] do
      existing_server = i > 1 ? " -gen_cluster servers '[#{SERVER}]' " : ""
      stop = ENV['STOP'] ? " -s init stop " : ""
      sh %Q{erl -pa #{root}/ebin -pa #{root}/deps/*/ebin  \
-name "example_cluster_srv#{i}@localhost" \
+W w \
-boot start_sasl \
-s reloader \
-s example_cluster_srv start \
#{existing_server}  \
#{stop}
      }, :verbose => true
    end
  end
end
