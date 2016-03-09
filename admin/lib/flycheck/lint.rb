# Copyright (c) 2016 Sebastian Wiesner and Flycheck contributors

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <http://www.gnu.org/licenses/>.

module Flycheck
  # Provides linting for Emacs Lisp
  module Lint
    module_function

    # Represents an error detected in a file
    class Error
      attr_reader :filename
      attr_reader :line
      attr_reader :kind
      attr_reader :column
      attr_reader :message

      def initialize(filename, kind, line, column, message)
        @filename = filename
        @kind = kind
        @line = line
        @column = column
        @message = message
      end

      def self.from_line(kind, line, column, message)
        Error.new(line.filename, kind, line.number, column, message)
      end

      def to_s
        if @column
          "#{@filename}:#{@kind}:#{@line}:#{@column}: #{@message}"
        else
          "#{@filename}:#{@kind}:#{@line}: #{@message}"
        end
      end
    end

    # A single line in a file
    class Line
      attr_reader :filename
      attr_reader :number
      attr_reader :text

      def initialize(filename, number, text)
        @filename = filename
        @number = number
        @text = text
      end

      def to_str
        @text
      end
    end

    # A file being linted
    class LintedFile
      attr_reader :filename

      def initialize(filename)
        @filename = filename
        @lines = File.new(filename).map.with_index(1) do |line, line_no|
          Line.new(@filename, line_no, line)
        end
      end

      def each
        return enum_for(:each) unless block_given?

        @lines.each do |line|
          yield line
        end
      end

      def each_error
        return enum_for(:each_error) unless block_given?

        Lints.methods(false).each do |lint|
          Lints.send(lint, self) do |error|
            yield error
          end
        end
      end

      def to_s
        "LintedFile(#{@filename})"
      end
    end

    def whitelisted?(error)
      # Filter "Argument should appear in docstring" errors for arguments that
      # are marked as unused by a leading underscore.  On Emacs 25 checkdoc
      # ignores these arguments, but Emacs 24 complains, hence let's filter
      # these faulty complaints.
      # TODO: Remove when dropping Emacs 24 support
      error.kind == :checkdoc &&
        /^Argument `_[^']+' should appear/ =~ error.message
    end

    def check_files(files)
      errors = files
               .lazy
               .flat_map { |f| LintedFile.new(f).each_error.lazy }
               .reject { |e| whitelisted? e }
               .force
      errors.each do |error|
        puts error
      end
      raise 'Style errors' if errors.any?
      puts "#{files.length} files linted, no style errors detected"
    end

    # Holds all lints
    module Lints
      module_function

      def check_tab_indentation(file)
        file.each do |line|
          tabs = /\t/.match(line.text)
          yield Error.from_line(:space, line,
                                tabs.begin(0) + 1, 'Tab found') if tabs
        end
      end

      def check_doc(file)
        command = ['-l', 'admin/run-checkdoc.el',
                   '-f', 'flycheck-checkdoc-batch-and-exit',
                   file.filename]
        IO.popen(Flycheck::Util.emacs_batch(*command)) do |source|
          source.each_line do |line|
            match = /^([^:]+):(\d+): (.+)$/.match(line)
            yield Error.new(match[1], :checkdoc,
                            match[2], nil, match[3]) if match
          end
        end
      end
    end
  end
end
