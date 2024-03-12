import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';
import marked from 'marked';

import path from 'path';
import puppeteer from 'puppeteer';
import fs from 'fs/promises';
import { existsSync } from 'fs';
import { Liquid } from 'liquidjs';

const liquid = new Liquid();

const svg = fs.readFile('./src/card.svg', 'utf8').then(svg => {
  return liquid.parse(svg);
});

async function path_exists(path: string) {
  try {
    await fs.access(path, fs.constants.R_OK | fs.constants.W_OK);
    return true;
  } catch (e) {
    return false;
  }
}

type RenderOptions = {
  title: string;
  fullname: string;
  avatar: string;
  slug: string;
};

const browser = puppeteer.launch({
  headless: true
});

const delay = (time: number) => new Promise(resolve => setTimeout(resolve, time));

async function render({ title, fullname, avatar, slug }: RenderOptions) {
  const svg_path = path.join(__dirname, 'static/img');
  const output_svg = await liquid.render(await svg, {
    fullname,
    title,
    avatar,
    date: 'N/A'
  });
  const svg_fullname = path.join(__dirname, 'tmp.svg');
  if (await path_exists(svg_fullname)) {
    await fs.unlink(svg_fullname);
  }
  await fs.writeFile(svg_fullname, output_svg);
  const directory = `build/img/`;
  if (!await path_exists(directory)) {
    await fs.mkdir(directory, { recursive: true });
  }
  const filename = `${directory}${slug}.png`;
  const page = await (await browser).newPage();
  await page.setViewport({
    height: 630,
    width: 1200
  });
  await page.goto('file://' + svg_fullname);
  await delay(100);

  const imageBuffer = await page.screenshot({});

  await fs.writeFile(filename, imageBuffer);

  console.log(`[Docusaurs] Writing ${filename}`);
  await page.close();
}

const isProd = process.env.NODE_ENV === 'production';

const config: Config = {
  title: 'LIPS Scheme',
  tagline: 'Powerfull Scheme based Lisp in JavaScript',
  favicon: 'img/favicon.ico',

  // Set the production url of your site here
  url: 'https://jcubic.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/lips-website/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'jcubic', // Usually your GitHub org/user name.
  projectName: 'lips-website', // Usually your repo name.
  deploymentBranch: 'docusaurus',


  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        blog: {
          showReadingTime: true,
          editUrl: 'https://github.com/jcubic/lips-website/tree/docusaurus/docs/',
          feedOptions: {
            type: 'rss',
            limit: 10,
            copyright: `Copyright © ${new Date().getFullYear()} Jakub T. Jankiewicz`,
            title: 'LIPS Scheme blog',
            description: 'LIPS Scheme blog RSS Feed',
            createFeedItems: async ({ blogPosts,...params }) => {
              if (isProd) {
                await Promise.all(blogPosts.map(async (blogPost) => {
                  const slug = blogPost.metadata.permalink.replace(/^.*\//, '');
                  const title = blogPost.metadata.title;
                  const fullname = 'Jakub T. Jankiewicz';
                  const avatar = 'https://github.com/jcubic.png';
                  await render({ title, fullname, avatar, slug });
                }));
              }
              const feedItems = await params.defaultCreateFeedItems({ blogPosts, ...params });
              feedItems.forEach((feedItem,index) => {
                const blogPost = blogPosts[index]!;
                const permalink = blogPost.metadata.permalink;
                const excerpt = blogPost.content.replace(/<!--\s*truncate\s*-->[\s\S]*$/, '').trim();
                const html = (marked.parse(excerpt) as string) + `<br/><a href="${permalink}">see the rest of the article</a>`
                feedItem.content = html;
              });
              return feedItems;
            }
          }
        },
        docs: {
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/jcubic/lips-website/tree/docusaurus/docs/',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    image: 'img/docusaurus-social-card.jpg',
    navbar: {
      title: 'Scheme',
      logo: {
        alt: 'LIPS Scheme Logo',
        src: 'img/logo-black.svg',
        srcDark: 'img/logo-white.svg',
        target: '_self',
        href: '/',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'tutorialSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {to: '/blog', label: 'Blog', position: 'left'},
        {
          href: 'https://twitter.com/LIPS_scheme',
          label: 'Twitter/X',
          position: 'right'
        },
        {
          href: 'https://gitter.im/jcubic/lips',
          label: 'Chat',
          position: 'right'
        },
        {
          href: 'https://github.com/jcubic/lips',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting Started',
              to: '/docs/intro',
            },
            {
              label: 'Introduction to Scheme',
              to: '/docs/scheme-intro/what-is-lisp'
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Gitter',
              href: 'https://gitter.im/jcubic/lips',
            },
            {
              label: 'Twitter',
              href: 'https://twitter.com/LIPS_scheme',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'Blog',
              to: '/blog',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/jcubic/lips',
            },
          ],
        },
      ],
      copyright: `Copyright (c) 2018-${new Date().getFullYear()} <a href="https://jakub.jankiewicz.org">Jakub T. Jankiewicz</a><br/>Website content licenses with <a href="https://creativecommons.org/licenses/by-sa/4.0/">CC BY-SA 4.0</a> unless noted otherwise`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['scheme', 'lisp']
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
