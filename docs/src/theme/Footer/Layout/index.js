import React from 'react';
import clsx from 'clsx';
import { useLocation } from '@docusaurus/router';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';

import MoreIcon from '@site/src/components/MoreIcon';
import './style.css';
const NotByAI = require('@site/static/img/Written-By-Human-Not-By-AI-Badge-black.svg').default

export default function FooterLayout({style, links, logo, copyright}) {
  const { pathname: currentPath } = useLocation();
  const { siteConfig: { baseUrl } } = useDocusaurusContext();

  return (
    <footer
      className={clsx('footer', {
        'footer--dark': style === 'dark',
      })}>
      <div className="container container-fluid">
        {links}
        {(logo || copyright) && (
          <div className="footer__bottom text--center">
            {logo && <div className="margin-bottom--sm">{logo}</div>}
            {copyright}
            <a href="https://notbyai.fyi" aria-label="Created not by AI"><NotByAI /></a>
          </div>
        )}
      </div>
      <script defer src="https://api.feedbhack.com/assets/app.js" website-id="6703059dee359a44f772ff78"></script>
      {baseUrl === currentPath && <MoreIcon />}
      <script dangerouslySetInnerHTML={{__html: `
var owa_baseUrl = 'https://stats.jcubic.pl/';
var owa_cmds = owa_cmds || [];
owa_cmds.push(['setSiteId', '922150e605063a8580e17047ffbf3ec2']);
owa_cmds.push(['trackPageView']);
owa_cmds.push(['trackClicks']);

(function() {
    var _owa = document.createElement('script'); _owa.type = 'text/javascript';
     _owa.async = true;
     owa_baseUrl = ('https:' == document.location.protocol ? window.owa_baseSecUrl || owa_baseUrl.replace(/http:/, 'https:') : owa_baseUrl );
    _owa.src = owa_baseUrl + 'modules/base/js/owa.tracker-combined-min.js';
    var _owa_s = document.getElementsByTagName('script')[0]; _owa_s.parentNode.insertBefore(_owa, _owa_s);
}());`}}/>
      <script dangerouslySetInnerHTML={{__html: `
     var _paq = _paq || [];
    _paq.push(['setTrackingSource', 'jstc_tm']);
    _paq.push(['enableLinkTracking']);
    _paq.push(['setIpTracking', true]);
    _paq.push(['setDomains', ['lips.js.org']]);
    _paq.push(['trackPageView']);
    _paq.push(['enableJSErrorTracking']);
    (function(p,i,w,ik) {
        var g=ik.createElement('script'),s=ik.getElementsByTagName('script')[0];
        _paq.push(['setTrackerUrl', p]);
        _paq.push(['setSiteId', w]);
        g.type='text/javascript';g.async=true;g.defer=true;g.src=i;s.parentNode.insertBefore(g,s);
    })('https://jcubic.piwik.pro/ppms.php','https://jcubic.containers.piwik.pro/ppms.js','018719ec\u002D6793\u002D4a2d\u002D92f9\u002Db8b88a01d43c',document)`}}/>
      <script defer src="https://cloud.umami.is/script.js" data-website-id="422bfe77-bcfe-4cf8-a923-d496fba1bcc3" />
    </footer>
  );
}
