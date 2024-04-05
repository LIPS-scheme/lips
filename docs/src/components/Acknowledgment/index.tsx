import Heading from '@theme/Heading';
import styles from '../styles.module.css';

export default function Acknowledgment(): JSX.Element {
    return (
      <div className={styles.section}>
        <Heading as="h2" id="acknowledgment" className={styles.header}>Acknowledgments</Heading>
        <ul>
          <li>Font used in logo is <a href="https://www.dafont.com/telegrafico.font">Telegrafico</a> by <a href="https://www.deviantart.com/ficod">ficod</a>.</li>
          <li>Current Parser is inspired by implementation in <a href="https://www.biwascheme.org/">BiwaScheme</a> by Yutaka HARA (yhara).</li>
          <li><code>fetch</code> polyfill use <a href="https://github.com/developit/unfetch">unfetch</a> by Jason Miller.</li>
          <li>Browser <code>init</code> function use <a href="http://javascript.nwbox.com/ContentLoaded/">ContentLoaded</a>.</li>
          <li>The rationalize algorithm is based on <a href="https://www.gnu.org/software/kawa/index.html">Kawa Scheme</a> by Per M.A. Bothner, Alan Bawden and Marc Feeley.</li>
          <li><code>ucs2decode</code> function taken from <a href="https://github.com/bestiejs/punycode.js">punycode.js</a> by <a href="https://mathiasbynens.be/">Mathias Bynens</a>.</li>
          <li><a href="https://rosettacode.org/">Rosetta Code</a> was used for:
            <ul dir="auto">
              <li><a href="https://rosettacode.org/wiki/Greatest_common_divisor#JavaScript">gdc</a>,</li>
              <li><a href="https://rosettacode.org/wiki/Least_common_multiple#JavaScript">lcm</a>,</li>
              <li><a href="https://rosettacode.org/wiki/Convert_decimal_number_to_rational">LFloat::toRational</a>.</li>
            </ul>
          </li>
          <li><a href="https://stackoverflow.com">StackOverlow</a> code was used for functions:
            <ul dir="auto">
              <li><a href="https://stackoverflow.com/a/10372280/387194">fworker</a>,</li>
              <li><a href="https://stackoverflow.com/a/27282907/387194">flatten</a>,</li>
              <li><a href="https://stackoverflow.com/a/4331218/387194">allPossibleCases</a>.</li>
            </ul>
          </li>
          <li>Code formatter is roughly based on <a href="http://community.schemewiki.org/?scheme-style">scheme-style</a> and GNU Emacs scheme mode.</li>
          <li>Some helpers in standard library are inspired by same functions from <a href="https://ramdajs.com/">RamdaJS library</a>.</li>
          <li>Special thanks to <a href="https://github.com/lassik">Lassi Kortela</a> for helping with Scheme code.</li>
        </ul>
      </div>
    );
}
