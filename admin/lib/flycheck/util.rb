# Copyright (c) 2012-2016 Sebastian Wiesner and Flycheck contributors

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
  # An unexpected environment
  class UnexpectedEnvironmentError < StandardError
  end

  # Utilities
  module Util
    # Create a command array to run +args+ with Emacs batch.
    def emacs_batch(*args)
      [ENV['EMACS'] || 'emacs', '-Q', '--batch'] + args
    end

    # Ensure that an environment +variable+ has an expected +value+.  Otherwise
    # +fail+ with the given +message+
    def self.check_environment(variable, value, message)
      raise UnexpectedEnvironmentError, message unless ENV[variable] == value
    end

    # Run a block with a safe 0077 umask
    def self.with_safe_umask
      old_umask = File.umask
      begin
        File.umask(0077)
        yield
      ensure
        File.umask(old_umask)
      end
    end
  end
end
