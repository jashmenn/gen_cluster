require "skelerl"
Dir.glob(File.dirname(__FILE__) + "/priv/tasks/*.rake").each {|f| load f}

Rake::TaskManager.class_eval do
  def remove_task(task_name)
    @tasks.delete(task_name.to_s)
  end
end
 
def remove_task(task_name)
  Rake.application.remove_task(task_name)
end


# TODOlist for generator
# add utils/make_boot
# generate an application
 
def ebin_dirs
  Dir[File.dirname(__FILE__) + "/ebin"]  +
  Dir[File.dirname(__FILE__) + "/**/deps/**/ebin"] +
  Dir[File.dirname(__FILE__) + "/test/ebin"]
end

def erl
  "erl"
end

task :boot => [:compile] do
  sh "(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts stoplight)"
  sh "(cd ebin; erl -pa ebin -noshell -run make_boot write_scripts stoplight_client)"
end

remove_task :compile
task :compile do
  sh "#{erl} -pa #{ebin_dirs} -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'", :verbose => true
end

task :default => [:compile]

task :rstakeout do
    cmd =  %Q{rstakeout -t 1 -v "rake run_tests --trace" '*/**/*.erl'}
    puts cmd
    exec cmd
end

