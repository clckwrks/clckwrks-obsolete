{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Theme.Page where

import Clckwrks
import Data.Text      (unpack)
import Theme.Template

page :: XMLGenT (Clck ClckURL) XML
page =
    do ttl <- lift getPageTitle
       template (unpack ttl) () $
           <%>
            <div id="page-content">
             <h1 class="page-title"><% getPageTitle %></h1>
             <% getPageContent %>
            </div>
           </%>
{-
<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css") />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>

  <% getMenu %>

  <div id="background-box">
  </div>

  <div id="banner-box">
   <div class="mesh"></div>
        
   <div class="img-text-bg"></div>
   <div class="img-text">The relentless, uncompromised power and beauty of Haskell for the web</div>
   <img src=(ThemeData "seven.png") />
--   <img src=(ThemeData "lyra.jpg") />
--   <img src=(ThemeData "gears.jpg") />
  </div>

  <blockquote>
   The relentless, uncompromised power and beauty of Haskell for the web
  </blockquote>
    <div class="summary-boxes">
     <div class="summary-box">
      <h2>Elegance</h2>
      <img src=(ThemeData "icons/dowload.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>

     <div class="summary-box">
      <h2>Speed</h2>
      <img src=(ThemeData "icons/phone.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>

     <div class="summary-box">
      <h2>Power</h2>
      <img src=(ThemeData "icons/usb.png") />
      <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec ut tortor non augue tincidunt iaculis. Cras ac diam rhoncus nibh commodo iaculis vel sed ligula. Curabitur fringilla tortor sed massa consequat convallis. Maecenas consectetur tincidunt porttitor. Aenean quis posuere augue.</p>
     </div>
    </div>

    <div id="footer">
     <div id="copyright">Powered by Happstack. Copyright 2012, Jeremy Shaw</div>
    </div>

 </body>
</html>

{-
<html>
 <head>
  <title><% getPageTitle %></title>
  <link rel="stylesheet" type="text/css" href=(ThemeData "style.css")    />
  <link rel="stylesheet" type="text/css" href=(ThemeData "hscolour.css") />
 </head>
 <body>
  <div id="clckwrks-menu">
    <span id="clck"><a href="/">Clck</a></span><span id="wrks"><a href="/">wrks</a></span><br />
    <span id="clckwrks-byline">for secure, reliable, & <br />integrated websites</span>
--    <% getPageMenu %>
    <% getMenu %>Te
  </div>

  <div id="clckwrks-body">
   <h1><% getPageTitle   %></h1>
   <p><%  getPageContent %></p>
  </div>
 </body>
</html>
-}
-}