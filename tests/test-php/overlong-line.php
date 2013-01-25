<?php

/**
 * Test a warning triggered by an overlong line.
 *
 * Checkers: php-phpcs
 *
 * The following comment blurp is to make PHPCS happy.
 *
 * PHP Version 5
 *
 * @category Testcase
 * @package  Flycheck
 * @author   Sebastian Wiesner <lunaryorn@gmail.com>
 * @license  http://www.gnu.org/licenses/gpl.html GPL-3
 * @link     https://github.com/lunaryorn/flycheck
 */

$spam = "This line is really, really long to trigger a warning about an overlong line"";

?>