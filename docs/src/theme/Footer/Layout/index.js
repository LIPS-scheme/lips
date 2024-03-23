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
            <a href="https://notbyai.fyi"><NotByAI /></a>
          </div>
        )}
      </div>
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
    </footer>
  );
}
