// @ts-check
// See: https://docusaurus.io/docs/api/docusaurus-config

import {themes as prismThemes} from 'prism-react-renderer';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const githubUrl = 'https://github.com';
const organizationName = 'sbcgua';
const projectName = 'ajson';
const repoUrl = `${githubUrl}/${organizationName}/${projectName}`

/** @type {import('@docusaurus/types').Config} */
const config = {
  title:   'AJson',
  tagline: 'Abap library to import/export and manipulate data in JSON format',
  favicon: 'img/favicon.ico',

  // Site URLs
  url: githubUrl,               // Set the production url of the site here
  baseUrl: `/${projectName}/`,  // Set the /<baseUrl>/ pathname under which your site is served, @github usually '/<projectName>/'

  // GitHub pages deployment config
  organizationName,   // GitHub org/user name
  projectName,        // Repo name

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      {
        docs: {
          sidebarPath: './sidebars.js',
        },
        blog: {
          showReadingTime: true,
          feedOptions: {
            type: ['rss', 'atom'],
            xslt: true,
          },
          // Useful options to enforce blogging best practices
          onInlineTags: 'warn',
          onInlineAuthors: 'warn',
          onUntruncatedBlogPosts: 'warn',
        },
        theme: {
          customCss: './src/css/custom.css',
        },
        gtag: {
          trackingID: 'G-E4721PW62J',
          anonymizeIP: true,
        },
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    {
      image: 'img/logo-128.png',
      navbar: {
        title: 'AJson',
        logo: {
          alt: 'AJson Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'tutorialSidebar',
            position: 'left',
            label: 'Documentation',
          },
          {
            to: '/blog',
            label: 'Blog',
            position: 'left'
          },
          {
            href: repoUrl,
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            label: 'Documentation',
            to: '/docs/intro',
          },
          {
            label: 'Blog',
            to: '/blog',
          },
          {
            label: 'GitHub',
            href: repoUrl,
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} Alexander Tsybulsky (aka sbcgua), Built with Docusaurus.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        additionalLanguages: ['abap'],
      },
      // algolia: {
      //   appId: 'JYRGWOZND5',
      //   apiKey: '7f7a2954f42dc2446a9c7bf3c7f305af',
      //   indexName: 'sbcguaio',
      //   contextualSearch: false, // No versions and languages so far

      //   // Optional: Specify domains where the navigation should occur through window.location instead on history.push. Useful when our Algolia config crawls multiple documentation sites and we want to navigate with window.location.href to them.
      //   // externalUrlRegex: 'external\\.com|domain\\.com',
      //   // Optional: Replace parts of the item URLs from Algolia. Useful when using the same search index for multiple deployments using a different baseUrl. You can use regexp or string in the `from` param. For example: localhost:3000 vs myCompany.com/docs
      //   // replaceSearchResultPathname: {
      //   //   from: '/docs/', // or as RegExp: /\/docs\//
      //   //   to: '/',
      //   // },

      //   // Optional: Algolia search parameters
      //   // searchParameters: {},

      //   // Optional: path for search page that enabled by default (`false` to disable it)
      //   searchPagePath: 'search',
      //   // Optional: whether the insights feature is enabled or not on Docsearch (`false` by default)
      //   insights: false,
      // },      
    },

    plugins: [
      [
        '@docusaurus/plugin-client-redirects',
        {
          redirects: [
            {
              from: ['/docs'],
              to: '/docs/intro',
            },
          ],
        },
      ],
    ],
};

export default config;
