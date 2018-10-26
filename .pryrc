# Settings
## Pry
### History
Pry.config.should_load_rc = false
Pry.config.history.file = '~/.pry_history'
Pry.hooks.add_hook(:before_session, 'simple-prompt') do |_output, _binding, pry|
  pry.push_prompt Pry::SIMPLE_PROMPT
end

# Pry.hooks.add_hook(:before_session, 'reload-history') do |output, _binding, pry|
#   output.puts Pry.config.history.file
#   Pry.history.load
# end

### Prompt
Pry.config.color = true
Pry.config.prompt = proc do |obj, _level, _|
 prompt = ''
 prompt << "#{Rails.version}@" if defined?(Rails)
 prompt << RUBY_VERSION
 "#{prompt} (#{obj})> "
end

### Alias
Pry.config.commands.alias_command 'lM', 'ls -M'
# Ever get lost in pryland? try w!
Pry.config.commands.alias_command 'w', 'whereami'
# Clear Screen
Pry.config.commands.alias_command '.clr', '.clear'

Pry.config.editor = 'emacsclient -c '

## AWESOME_PRINT
# refs: https://github.com/pry/pry/wiki/FAQ#wiki-awesome_print
begin
  require 'awesome_print'
  Pry.config.print = proc { |output, value| Pry::Helpers::BaseHelpers.stagger_output("=> #{value.ai}", output) }
# Pry.config.print = proc { |output, value| output.puts value.ai } #ページングなし
rescue LoadError => err
  puts 'no awesome_print :('
  puts err
end

# refs: https://github.com/pry/pry/wiki/FAQ#wiki-hirb
begin
  require 'hirb'
rescue LoadError => _err
  puts 'no hirb :('
end

if defined? Hirb
  Hirb::View.instance_eval do
    def enable_output_method
      @output_method = true
      @old_print = Pry.config.print
      Pry.config.print = proc do |output, value|
        Hirb::View.view_or_page_output(value) || @old_print.call(output, value)
      end
    end

    def disable_output_method
      Pry.config.print = @old_print
      @output_method = nil
    end
  end
  Hirb.enable
end

# refs: https://github.com/deivid-rodriguez/pry-byebug#matching-byebug-behaviour
if defined?(PryByebug)
  Pry.commands.alias_command 'c', 'continue'
  Pry.commands.alias_command 's', 'step'
  Pry.commands.alias_command 'n', 'next'
  Pry.commands.alias_command 'f', 'finish'
end
