# ----------------------------------------- #
## Rakefile for Erlang Twitter Application ##
# ----------------------------------------- #

require 'rake/clean'


##
# Define Constants

APP_PATH = '.'
SRC_DIR = File.join(APP_PATH, 'src')
EBIN_DIR = File.join(APP_PATH, 'ebin')
INCLUDE_DIR = File.join(APP_PATH, 'include')
PRIV_DIR = File.join(APP_PATH, 'priv')
TEST_DIR = File.join(APP_PATH, 'test')
ERLC = 'erlc -W0 -o ' + EBIN_DIR + ' '


##
# AutoMake Directories

directory EBIN_DIR
directory INCLUDE_DIR
directory PRIV_DIR


##
# Cleaning Settings

CAN_CLEAN_BEAMS = FileList[File.join(EBIN_DIR, '*.beam')]
CLEAN.include(CAN_CLEAN_BEAMS)


##
# Tasks

desc 'Default'
task default: 'clean_compile'

desc 'Clean Compile'
task clean_compile: [:init, :compile]

desc 'Rake Cleaning'
task :init do
	Rake::Task['clean'].invoke
end

desc 'Compile'
task compile: [:app]

desc 'Test Source Compile'
task test: [:testfiles]


##
# File Tasks

file app: FileList[File.join(SRC_DIR, '*.erl')] do |t|
	sh ERLC + t.prerequisites.join(' ')
end

file testfiles: FileList[File.join(TEST_DIR, '*.erl')] do |t|
	sh ERLC + t.prerequisites.join(' ')
end
