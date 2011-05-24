require 'rake'
require 'rake/clean'

ERLC = 'erlc'
ERLC_ARGS = ''
ERLC_ARGS_GEN = '+inline +native +{hipe,[o3]}'
CC_ARGS = '-O3 -ffast-math -fPIC -shared -Wall'

BEAM_OUT = 'ebin'
SHLIB_OUT = BEAM_OUT
GEN_OUT = 'tmp'
INCLUDE = 'include'

CLEAN.include([
  "#{GEN_OUT}/*.erl",
  "#{BEAM_OUT}/*.beam",
  "#{SHLIB_OUT}/*.so",
  '*.dump'
])

SRC_ERL = FileList['src/**/*.erl']
SRC_GEN = FileList['src/**/*.xrl', 'src/**/*.yrl']
SRC_C = FileList['src/**/*.c']
SRC_ERL_GEN = []
DST_BEAM = []
DST_SHLIB = []

SRC_GEN.each do |src_gen|
  src_erl_gen = File.join(GEN_OUT, File.basename(src_gen).ext('erl'))
  file src_erl_gen => [src_gen] do
    sh "#{ERLC} -v -o #{GEN_OUT} #{src_gen}"
  end
  SRC_ERL_GEN << src_erl_gen
end

SRC_ERL_GEN.each do |src_erl_gen|
  beam = File.join(BEAM_OUT, File.basename(src_erl_gen).ext('beam'))
  file beam => [src_erl_gen] do
    sh "#{ERLC} -v -I #{INCLUDE} -o #{BEAM_OUT} #{ERLC_ARGS_GEN} #{src_erl_gen}"
  end
  DST_BEAM << beam
end

SRC_ERL.each do |src_erl|
  beam = File.join(BEAM_OUT, File.basename(src_erl).ext('beam'))
  file beam => [src_erl] do
    sh "#{ERLC} -v -I #{INCLUDE} -o #{BEAM_OUT} #{ERLC_ARGS} #{src_erl}"
  end
  DST_BEAM << beam
end

SRC_C.each do |src_c|
  shlib = File.join(SHLIB_OUT, File.basename(src_c).ext('so'))
  file shlib => [src_c] do |f|
    sh "gcc #{CC_ARGS} -o #{f.name} #{src_c}"
  end
  DST_SHLIB << shlib
end

namespace :erlang do
  desc "run" 
  task :run => [:compile] do
    sh("erl -noshell -pa #{BEAM_OUT} -s mymod start")
  end

  desc "test" 
  task :test => [:compile] do
    sh("erl -noshell -pa #{BEAM_OUT} -s mymod test -s init stop")
  end
end

task :default => [:compile]
task :start => ['erlang:run']
task :compile => DST_BEAM
task :compile => DST_SHLIB
