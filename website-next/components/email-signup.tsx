import React from 'react'

export const BasicEmailSignup = React.memo(() => {
  return (
    <div
      dangerouslySetInnerHTML={{
        __html: `
        <!-- Begin Mailchimp Signup Form -->
        <link href="//cdn-images.mailchimp.com/embedcode/horizontal-slim-10_7.css" rel="stylesheet" type="text/css">
        <div id="mc_embed_signup">
        <form action="https://app.us6.list-manage.com/subscribe/post?u=45910e347a2446abcf18e9b45&amp;id=30e94ed0b5" method="post" id="mc-embedded-subscribe-form" name="mc-embedded-subscribe-form" class="validate" target="_blank" novalidate>
            <div id="mc_embed_signup_scroll">
          <label for="mce-EMAIL" className='font-normal'></label>
          <input type="email" value="" name="EMAIL" class="email" id="mce-EMAIL" placeholder="email address" required>
            <!-- real people should not fill this in and expect good things - do not remove this or risk form bot signups-->
            <div style="position: absolute; left: -5000px;" aria-hidden="true"><input type="text" name="b_45910e347a2446abcf18e9b45_30e94ed0b5" tabindex="-1" value=""></div>
            <div class="clear"><input type="submit" value="Subscribe" name="subscribe" id="mc-embedded-subscribe" class="button"></div>
            </div>
        </form>
        </div>
        
        <!--End mc_embed_signup-->
      `,
      }}
    />
  )
})
