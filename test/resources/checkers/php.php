<?php

/**
 * Test warnings for PHPCS and 
 *
 * Checkers: php-phpcs
 *
 * The following comment blurp makes PHPCS really happy :)
 *
 * PHP Version 5
 *
 * @category Testcase
 * @package  Flycheck
 * @author   Sebastian Wiesner <swiesner@lunaryorn.com>
 * @license  http://www.gnu.org/licenses/gpl.html GPL-3
 * @link     https://github.com/flycheck/flycheck
 */

class A
{
    private static $FOO = 2;

    /** Does nothing useful */
    private function bar($baz)
    {
        $i=FALSE;
    }
}

?>
