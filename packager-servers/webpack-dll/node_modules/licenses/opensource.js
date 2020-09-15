'use strict';

/**
 * Contains a mapping for license id -> license name.
 *
 * @type {Object}
 * @public
 */
exports.types = Object.create(null);

/**
 * Contains a mapping for license name -> license object.
 *
 * @type {Object}
 * @public
 */
exports.licenses = Object.create(null);

/**
 * The full license information.
 *
 * @type {Array}
 * @public
 */
exports.full = [
  {
    id: 'AFL2',
    name: 'AFL 2.1',
    full: 'Academic Free License 2.1',
    file: 'AFL2.1.txt'
  },
  {
    id: 'AFL3',
    name: 'AFL 3.0',
    full: 'Academic Free License 3.0',
    url: 'http://opensource.org/licenses/AFL-3.0',
    tldr: 'https://tldrlegal.com/license/academic-free-license-3.0-(afl)',
    file: 'AFL3.0.txt'
  },
  {
    id: 'AGPL3',
    name: 'AGPL 3.0',
    full: 'GNU Affero General Public License 3.0',
    url: 'http://opensource.org/licenses/AGPL-3.0',
    tldr: 'https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)',
    file: 'AGPL3.0.txt'
  },
  {
    id: 'CCBY4',
    name: 'CC 4.0 BY',
    full: 'Creative Commons Attribution 4.0',
    url: 'http://creativecommons.org/licenses/by/4.0/'
  },
  {
    id: 'CCBYND4',
    name: 'CC 4.0 BY-ND',
    full: 'Creative Commons Attribution NoDerivs 4.0',
    url: 'http://creativecommons.org/licenses/by-nd/4.0/'
  },
  {
    id: 'CCBYSA4',
    name: 'CC 4.0 BY-SA',
    full: 'Creative Commons Attribution ShareAlike 4.0',
    url: 'http://creativecommons.org/licenses/by-sa/4.0/'
  },
  {
    id: 'CCBYNC4',
    name: 'CC 4.0 BY-NC',
    full: 'Creative Commons Attribution Non Commerical 4.0',
    url: 'http://creativecommons.org/licenses/by-nc/4.0/'
  },
  {
    id: 'CCBYNCSA',
    name: 'CC 4.0 BY-NC-SA',
    full: 'Creative Commons Attribution Non Commerical ShareAlike 4.0',
    url: 'http://creativecommons.org/licenses/by-nc-sa/4.0/'
  },
  {
    id: 'CCBYNCND4',
    name: 'CC 4.0 BY-NC-ND',
    full: 'Creative Commons Attribution Non Commerical NoDerivs 4.0',
    url: 'http://creativecommons.org/licenses/by-nc-nd/4.0/'
  },
  {
    id: 'CCBY3',
    name: 'CC 3.0 BY',
    full: 'Creative Commons Attribution 3.0',
    url: 'http://creativecommons.org/licenses/by/3.0/'
  },
  {
    id: 'CCBYND3',
    name: 'CC 3.0 BY-ND',
    full: 'Creative Commons Attribution NoDerivs 3.0',
    url: 'http://creativecommons.org/licenses/by-nd/3.0/'
  },
  {
    id: 'CCBYSA3',
    name: 'CC 3.0 BY-SA',
    full: 'Creative Commons Attribution ShareAlike 3.0',
    url: 'http://creativecommons.org/licenses/by-sa/3.0/'
  },
  {
    id: 'CCBYNC3',
    name: 'CC 3.0 BY-NC',
    full: 'Creative Commons Attribution Non Commerical 3.0',
    url: 'http://creativecommons.org/licenses/by-nc/3.0/'
  },
  {
    id: 'CCBYNCSA3',
    name: 'CC 3.0 BY-NC-SA',
    full: 'Creative Commons Attribution Non Commerical ShareAlike 3.0',
    url: 'http://creativecommons.org/licenses/by-nc-sa/3.0/'
  },
  {
    id: 'CCBYNCND3',
    name: 'CC 3.0 BY-NC-ND',
    full: 'Creative Commons Attribution Non Commerical NoDerivs 3.0',
    url: 'http://creativecommons.org/licenses/by-nc-nd/3.0/'
  },
  {
    id: 'CCBY25',
    name: 'CC 2.5 BY',
    full: 'Creative Commons Attribution 2.5',
    url: 'http://creativecommons.org/licenses/by/2.5/'
  },
  {
    id: 'CCBYND25',
    name: 'CC 2.5 BY-ND',
    full: 'Creative Commons Attribution NoDerivs 2.5',
    url: 'http://creativecommons.org/licenses/by-nd/2.5/'
  },
  {
    id: 'CCBYSA25',
    name: 'CC 2.5 BY-SA',
    full: 'Creative Commons Attribution ShareAlike 2.5',
    url: 'http://creativecommons.org/licenses/by-sa/2.5/'
  },
  {
    id: 'CCBYNC25',
    name: 'CC 2.5 BY-NC',
    full: 'Creative Commons Attribution Non Commerical 2.5',
    url: 'http://creativecommons.org/licenses/by-nc/2.5/'
  },
  {
    id: 'CCBYNCSA25',
    name: 'CC 2.5 BY-NC-SA',
    full: 'Creative Commons Attribution Non Commerical ShareAlike 2.5',
    url: 'http://creativecommons.org/licenses/by-nc-sa/2.5/'
  },
  {
    id: 'CCBYNCND25',
    name: 'CC 2.5 BY-NC-ND',
    full: 'Creative Commons Attribution Non Commerical NoDerivs 2.5',
    url: 'http://creativecommons.org/licenses/by-nc-nd/2.5/'
  },
  {
    id: 'APL1',
    name: 'APL 1.0',
    full: 'Adaptive Public License',
    url: 'http://opensource.org/licenses/APL-1.0',
    file: 'APL-1.0.txt'
  },
  {
    id: 'ARTISTIC2',
    name: 'Artistic 2.0',
    full: 'Artistic license, Version 2.0',
    url: 'http://opensource.org/licenses/Artistic-2.0',
    tldr: 'https://tldrlegal.com/license/artistic-license-2.0-(artistic)',
    file: 'Artistic2.0.txt'
  },
  {
    id: 'Apache2',
    name: 'Apache 2.0',
    full: 'Apache License, Version 2.0',
    url: 'http://opensource.org/licenses/Apache-2.0',
    tldr: 'https://tldrlegal.com/license/apache-license-2.0-(apache-2.0)',
    file: 'Apache2.0.txt'
  },
  {
    id: 'APSL2',
    name: 'Apple 2.0',
    full: 'Apple Public Source License 2.0',
    url: 'http://opensource.org/licenses/APSL-2.0',
    tldr: 'https://tldrlegal.com/license/apple-public-source-license-2.0-(apsl)',
    file: 'APSL-2.0.txt'
  },
  {
    id: 'AAL',
    name: 'AAL',
    full: 'Attribution Assurance License',
    url: 'http://opensource.org/licenses/AAL',
    file: 'AAL.txt'
  },
  {
    id: 'Apache',
    name: 'Apache',
    full: 'Apache License, Version 1.0',
    url: 'http://www.apache.org/licenses/LICENSE-1.0',
    file: 'Apache1.0.txt'
  },
  {
    id: 'BEER',
    name: 'Beerware',
    full: 'BEER-WARE License',
    url: 'http://en.wikipedia.org/wiki/Beerware',
    tldr: 'https://tldrlegal.com/license/beerware-license',
    file: 'beerware.txt'
  },
  {
    id: 'BSD2',
    name: 'BSD 2-Clause',
    full: 'BSD 2-Clause "Simplified" or "FreeBSD" License',
    url: 'http://opensource.org/licenses/BSD-2-Clause',
    tldr: 'https://tldrlegal.com/license/bsd-2-clause-license-(freebsd)',
    file: 'BSD-2-Clause.txt'
  },
  {
    id: 'BSD3',
    name: 'BSD 3-Clause',
    full: 'BSD 3-Clause "New" or "Revised" License',
    url: 'http://opensource.org/licenses/BSD-3-Clause',
    tldr: 'https://tldrlegal.com/license/bsd-3-clause-license-(revised)',
    file: 'BSD-3-Clause.txt'
  },
  {
    id: 'BSD',
    name: 'BSD 4-Clause',
    full: 'BSD 4-Clause or Original BSD license',
    url: 'http://en.wikipedia.org/wiki/BSD_licenses#4-clause_license_.28original_.22BSD_License.22.29',
    file: 'BSD.txt'
  },
  {
    id: 'BSL1',
    name: 'BSL 1.0',
    full: 'Boost Software License',
    url: 'http://opensource.org/licenses/BSL-1.0',
    tldr: 'https://tldrlegal.com/license/boost-software-license-1.0-explained',
    file: 'BSL1.0.txt'
  },
  {
    id: 'CECILL21',
    name: 'CECILL-2.1',
    full: 'Cea Cnrs Inria Logiciel Libre License, version 2.1',
    url: 'http://opensource.org/licenses/CECILL-2.1',
    file: 'CECILL-2.1.txt'
  },
  {
    id: 'CATOSL11',
    name: 'CATOSL-1.1',
    full: 'Computer Associates Trusted Open Source License 1.1',
    url: 'http://opensource.org/licenses/CATOSL-1.1',
    file: 'CATOSL1.1.txt'
  },
  {
    id: 'CDDL1',
    name: 'CDDL 1.0',
    full: 'Common Development and Distribution License Version 1.0',
    url: 'http://opensource.org/licenses/CDDL-1.0',
    tldr: 'https://tldrlegal.com/license/common-development-and-distribution-license-(cddl-1.0)-explained',
    file: 'cddl1.txt'
  },
  {
    id: 'CPAL1',
    name: 'CPAL 1.0',
    full: 'Common Public Attribution License Version 1.0',
    url: 'http://opensource.org/licenses/CPAL-1.0',
    tldr: 'https://tldrlegal.com/license/common-public-attribution-license-version-1.0-(cpal-1.0)',
    file: 'CPAL1.0.txt'
  },
  {
    id: 'CUAOPL1',
    name: 'CUA-OPL 1.0',
    full: 'CUA Office Public License Version 1.0',
    url: 'http://opensource.org/licenses/CUA-OPL-1.0',
    file: 'CUAOPL1.0.txt'
  },
  {
    id: 'EUDATAGRID',
    name: 'EUDatagrid',
    full: 'EU DataGrid Software License',
    url: 'http://opensource.org/licenses/EUDatagrid',
    file: 'EUDATAGRID.txt'
  },
  {
    id: 'EPL1',
    name: 'EPL 1.0',
    full: 'Eclipse Public License 1.0',
    url: 'http://opensource.org/licenses/EPL-1.0',
    tldr: 'https://tldrlegal.com/license/eclipse-public-license-1.0-(epl-1.0)',
    file: 'EPL-1.0.txt'
  },
  {
    id: 'ECL2',
    name: 'ECL 2.0',
    full: 'Educational Community License, Version 2.0',
    url: 'http://opensource.org/licenses/ECL-2.0',
    file: 'ECL2.0.txt'
  },
  {
    id: 'EFL2',
    name: 'EFL 2.0',
    full: 'Eiffel Forum License, Version 2',
    url: 'http://opensource.org/licenses/EFL-2.0',
    file: 'EFL2.0.txt'
  },
  {
    id: 'ENTESSA',
    name: 'Entessa',
    full: 'Entessa Public License Version. 1.0',
    url: 'http://opensource.org/licenses/Entessa',
    file: 'ENTESSA.txt'
  },
  {
    id: 'EUPL1',
    name: 'EUPL 1.1',
    full: 'European Union Public License, version 1.1',
    url: 'https://joinup.ec.europa.eu/system/files/EN/EUPL%20v.1.1%20-%20Licence.pdf',
    tldr: 'https://tldrlegal.com/license/european-union-public-licence',
    file: 'EUPL1.1.txt'
  },
  {
    id: 'FAIR',
    name: 'Fair',
    full: 'Fair License',
    url: 'http://opensource.org/licenses/Fair',
    tldr: 'https://tldrlegal.com/license/fair-license',
    file: 'FAIR.txt'
  },
  {
    id: 'FRAMEWORX',
    name: 'Frameworx 1.0',
    full: 'The Frameworx Open License 1.0',
    url: 'http://opensource.org/licenses/Frameworx-1.0',
    file: 'Frameworx1.0.txt'
  },
  {
    id: 'GPL2',
    name: 'GPL 2.0',
    full: 'GNU General Public License version 2.0',
    url: 'http://opensource.org/licenses/GPL-2.0',
    tldr: 'https://tldrlegal.com/license/gnu-general-public-license-v2',
    file: 'GPL-2.0.txt'
  },
  {
    id: 'GPL3',
    name: 'GPL 3.0',
    full: 'GNU General Public License version 3.0',
    url: 'http://opensource.org/licenses/GPL-3.0',
    tldr: 'https://tldrlegal.com/license/gnu-general-public-license-v3-(gpl-3)',
    file: 'GPL-3.0.txt'
  },
  {
    id: 'GPL',
    name: 'GPL',
    full: 'GNU General Public License',
    url: 'https://www.gnu.org/copyleft/gpl.html',
    file: 'GPL.txt'
  },
  {
    id: 'ISC',
    name: 'ISC',
    full: 'ISC License',
    url: 'http://opensource.org/licenses/ISC',
    tldr: 'https://tldrlegal.com/license/-isc-license',
    file: 'ISC.txt'
  },
  {
    id: 'LPPL13',
    name: 'LPPL 1.3c',
    full: 'LaTeX Project Public License, Version 1.3c',
    url: 'http://opensource.org/licenses/LPPL-1.3c',
    file: 'LPPL-1.3.txt'
  },
  {
    id: 'LPL',
    name: 'LPL 1.02',
    full: 'Lucent Public License Version 1.02',
    url: 'http://opensource.org/licenses/LPL-1.02',
    file: 'LPL-1.02.txt'
  },
  {
    id: 'MIROS',
    name: 'MirOS',
    full: 'MirOS License',
    url: 'http://opensource.org/licenses/MirOS',
    file: 'MIROS.txt'
  },
  {
    id: 'MSPL',
    name: 'MS-PL',
    full: 'Microsoft Public License',
    url: 'http://opensource.org/licenses/MS-PL',
    file: 'MS-PL.txt'
  },
  {
    id: 'MSRL',
    name: 'MS-RL',
    full: 'Microsoft Reciprocal License',
    url: 'http://opensource.org/licenses/MS-RL',
    file: 'MS-RL.txt'
  },
  {
    id: 'JSON',
    name: 'JSON.org',
    full: 'The JSON License',
    url: 'http://www.json.org/license.html',
    file: 'JSON.txt'
  },
  {
    id: 'LGPL2',
    name: 'LGPL 2.1',
    full: 'GNU Library or "Lesser" General Public License version 2.1',
    url: 'http://opensource.org/licenses/LGPL-2.1',
    tldr: 'https://tldrlegal.com/license/gnu-lesser-general-public-license-v2.1-(lgpl-2.1)',
    file: 'LGPL-2.1.txt'
  },
  {
    id: 'LGPL3',
    name: 'LGPL 3.0',
    full: 'GNU Library or "Lesser" General Public License version 3.0',
    url: 'http://opensource.org/licenses/LGPL-3.0',
    file: 'LGPL-3.0.txt'
  },
  {
    id: 'LGPL',
    name: 'LGPL',
    full: 'GNU Lesser General Public License',
    url: 'https://www.gnu.org/licenses/lgpl.html',
    file: 'LGPL.txt'
  },
  {
    id: 'HPND',
    name: 'HPND',
    full: 'Historical Permission Notice and Disclaimer',
    url: 'http://opensource.org/licenses/HPND',
    file: 'HPND.txt'
  },
  {
    id: 'IPL1',
    name: 'IPL 1.0',
    full: 'IBM Public License Version 1.0',
    url: 'http://opensource.org/licenses/IPL-1.0',
    tldr: 'https://tldrlegal.com/license/ibm-public-license-1.0-(ipl)',
    file: 'IPL-1.0.txt'
  },
  {
    id: 'IPA',
    name: 'IPA',
    full: 'IPA Font License Agreement v1.0',
    url: 'http://opensource.org/licenses/IPA',
    tldr: 'https://tldrlegal.com/license/ipa-font-license-(ipa)',
    file: 'IPA.txt'
  },
  {
    id: 'MIT',
    name: 'MIT',
    full: 'The MIT License',
    url: 'http://opensource.org/licenses/MIT',
    tldr: 'https://tldrlegal.com/license/mit-license',
    file: 'MIT.txt'
  },
  {
    id: 'MOTOSOTO',
    name: 'Motosoto',
    full: 'Motosoto Open Source License',
    url: 'http://opensource.org/licenses/Motosoto',
    file: 'MOTOSOTO.txt'
  },
  {
    id: 'MPL2',
    name: 'MPL 2.0',
    full: 'Mozilla Public License 2.0',
    url: 'http://opensource.org/licenses/MPL-2.0',
    tldr: 'https://tldrlegal.com/license/mozilla-public-license-2.0-(mpl-2)',
    file: 'MPL-2.0.txt'
  },
  {
    id: 'MPL',
    name: 'MPL 1.0',
    full: 'Mozilla Public License Version 1.0',
    url: 'http://opensource.org/licenses/MPL-1.0',
    file: 'MPL-1.0.txt'
  },
  {
    id: 'MULTICS',
    name: 'Multics',
    full: 'Multics License',
    url: 'http://opensource.org/licenses/Multics',
    file: 'MULTICS.txt'
  },
  {
    id: 'NASA1',
    name: 'NASA 1.3',
    full: 'NASA Open Source Agreement v1.3',
    url: 'http://opensource.org/licenses/NASA-1.3',
    file: 'nasa.txt'
  },
  {
    id: 'NTP',
    name: 'NTP',
    full: 'NTP License',
    url: 'http://opensource.org/licenses/NTP',
    file: 'NTP.txt'
  },
  {
    id: 'NAUMEN',
    name: 'NAUMEN',
    full: 'NAUMEN Public License',
    url: 'http://opensource.org/licenses/Naumen',
    file: 'NAUMEN.txt'
  },
  {
    id: 'NGPL',
    name: 'NGPL',
    full: 'The Nethack General Public License',
    url: 'http://opensource.org/licenses/NGPL',
    file: 'NGPL.txt'
  },
  {
    id: 'NOKIA',
    name: 'Nokia',
    full: 'Nokia Open Source License',
    url: 'http://opensource.org/licenses/Nokia',
    file: 'Nokia.txt'
  },
  {
    id: 'NPOLSL30',
    name: 'NPOSL 3.0',
    full: 'Non-Profit Open Software License 3.0',
    url: 'http://opensource.org/licenses/NPOSL-3.0',
    file: 'NPOSL-3.0.txt'
  },
  {
    id: 'OCLC2',
    name: 'OCLC 2.0',
    full: 'The OCLC Research Public License 2.0 License',
    url: 'http://opensource.org/licenses/OCLC-2.0',
    file: 'OCLC-2.0.txt'
  },
  {
    id: 'OFL11',
    name: 'OFL 1.1',
    full: 'SIL Open Font License',
    url: 'http://opensource.org/licenses/OFL-1.1',
    tldr: 'https://tldrlegal.com/license/open-font-license-(ofl)-explained',
    file: 'OFL-1.1.txt'
  },
  {
    id: 'OSL30',
    name: 'OSL 3.0',
    full: 'Open Software License v. 3.0',
    url: 'http://opensource.org/licenses/OSL-3.0',
    file: 'OSL-3.0.txt'
  },
  {
    id: 'PHP30',
    name: 'PHP 3.0',
    full: 'PHP License 3.0',
    url: 'http://opensource.org/licenses/PHP-3.0',
    tldr: 'https://tldrlegal.com/license/php-license-3.0-(php)',
    file: 'PHP-3.0.txt'
  },
  {
    id: 'POSTGRESQL',
    name: 'PostgreSQL',
    full: 'PostgreSQL Licence',
    url: 'http://opensource.org/licenses/PostgreSQL',
    file: 'PostgreSQL.txt'
  },
  {
    id: 'PROPRIETARY',
    name: 'Proprietary',
    full: 'Proprietary'
  },
  {
    id: 'PSF',
    name: 'Python 2.0',
    full: 'Python Software Foundation License Version 2.0',
    url: 'http://opensource.org/licenses/Python-2.0',
    tldr: 'https://tldrlegal.com/license/python-license-2.0',
    file: 'Python2.txt'
  },
  {
    id: 'CNRI',
    name: 'CNRI Python',
    full: 'CNRI portion of the multi-part Python License',
    url: 'http://opensource.org/licenses/CNRI-Python',
    file: 'CNRI.txt'
  },
  {
    id: 'QPL',
    name: 'QPL 1.0',
    full: 'Q Public License Version 1.0',
    url: 'http://opensource.org/licenses/QPL-1.0',
    file: 'QPL-1.0.txt'
  },
  {
    id: 'RPSL',
    name: 'RPSL 1.0',
    full: 'RealNetworks Public Source License Version 1.0',
    url: 'http://opensource.org/licenses/RPSL-1.0',
    file: 'RPSL.txt'
  },
  {
    id: 'RPL',
    name: 'RPL 1.5',
    full: 'Reciprocal Public License 1.5',
    url: 'http://opensource.org/licenses/RPL-1.5',
    file: 'RPL-1.5.txt'
  },
  {
    id: 'RSCPL',
    name: 'RSCPL',
    full: 'Ricoh Source Code Public License',
    url: 'http://opensource.org/licenses/RSCPL',
    file: 'RSCPL.txt'
  },
  {
    id: 'SIMPL',
    name: 'SimPL 2.0',
    full: 'Simple Public License 2.0',
    url: 'http://opensource.org/licenses/SimPL-2.0',
    tldr: 'https://tldrlegal.com/license/simple-public-license-2.0-(simpl)',
    file: 'SIMPL-2.0.txt'
  },
  {
    id: 'SLEEPYCAT',
    name: 'Sleepycat',
    full: 'Sleepycat License',
    url: 'http://opensource.org/licenses/Sleepycat',
    tldr: 'https://tldrlegal.com/license/sleepycat-license',
    file: 'SLEEPYCAT.txt'
  },
  {
    id: 'SPL',
    name: 'SPL 1.0',
    full: 'Sun Public License, Version 1.0',
    url: 'http://opensource.org/licenses/SPL-1.0',
    file: 'SPL-1.0.txt'
  },
  {
    id: 'WATCOM',
    name: 'Watcom 1.0',
    full: 'Sybase Open Source Watcom Public License Version 1.0',
    url: 'http://opensource.org/licenses/Watcom-1.0',
    file: 'Watcom-1.0.txt'
  },
  {
    id: 'NCSA',
    name: 'NCSA',
    full: 'The University of Illinois/NCSA Open Source License',
    url: 'http://opensource.org/licenses/NCSA',
    tldr: 'https://tldrlegal.com/license/university-of-illinois---ncsa-open-source-license-(ncsa)',
    file: 'NCSA.txt'
  },
  {
    id: 'VSL-1.0',
    name: 'VSL 1.0',
    full: 'The Vovida Software License v. 1.0',
    url: 'http://opensource.org/licenses/VSL-1.0',
    file: 'VSL-1.0.txt'
  },
  {
    id: 'W3C',
    name: 'W3C',
    full: 'W3C® Software Notice and License',
    url: 'http://opensource.org/licenses/W3C',
    file: 'W3C.txt'
  },
  {
    id: 'WXWINDOWS',
    name: 'WXwindows',
    full: 'wxWindows Library Licence',
    url: 'http://opensource.org/licenses/WXwindows',
    file: 'WXwindows.txt'
  },
  {
    id: 'XNET',
    name: 'Xnet',
    full: 'X.Net, Inc. License',
    url: 'http://opensource.org/licenses/Xnet',
    file: 'XNet.txt'
  },
  {
    id: 'ZPL20',
    name: 'ZPL 2.0',
    full: 'Zope Public License (ZPL) Version 2.0',
    url: 'http://opensource.org/licenses/ZPL-2.0',
    file: 'ZPL-2.0.txt'
  },
  {
    id: 'PUBLIC',
    name: 'Public Domain',
    full: 'Public Domain'
  },
  {
    id: 'UNLICENSE',
    name: 'Unlicense',
    full: 'Unlicense',
    url: 'http://unlicense.org',
    file: 'UNLICENSE.txt'
  },
  {
    id: 'WTFPL',
    name: 'WTFPL',
    full: 'Do What the Fuck You Want to Public License',
    url: 'http://www.wtfpl.net',
    tldr: 'https://tldrlegal.com/license/do-wtf-you-want-to-public-license-v2-(wtfpl-2.0)',
    file: 'WTFPL.txt'
  },
  {
    id: 'ZLIB',
    name: 'zlib/libpng',
    full: 'zlib/libpng License',
    url: 'http://opensource.org/licenses/Zlib',
    tldr: 'https://tldrlegal.com/license/zlib-libpng-license-(zlib)',
    file: 'zlib.txt'
  }
].map(function each(license) {
  exports.types[license.id] = license.name;
  exports.licenses[license.name] = license;

  return license;
});
