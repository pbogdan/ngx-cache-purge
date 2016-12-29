{-# LANGUAGE OverloadedStrings #-}

module PurgeSpec (main, spec) where

import           Protolude.Lifted

import           Cache.Content
import           Cache.Purge
import           Control.Monad (forM_)
import qualified Data.Set as Set
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "purePurge" $
  forM_ cases $ \(purgePath, purgeCount) ->
    it (show purgePath ++ " should purge " ++ show purgeCount ++ " items") $
    Set.size (fst $ runIdentity (purge purgePath ces)) `shouldBe` purgeCount
  where
    paths =
      [ "/"
      , "/favicon.ico?"
      , "/foo"
      , "/foo/"
      , "/foo/?"
      , "/foo/?blah=1"
      , "/wp-admin/css/login.min.css??3.9.1"
      , "/wp-admin/images/wordpress-logo.svg??20131107"
      , "/wp-content/plugins/akismet/_inc/akismet.css??3.0.0"
      , "/wp-content/plugins/akismet/_inc/akismet.js??3.0.0"
      , "/wp-content/plugins/akismet/_inc/form.js??3.0.0"
      , "/wp-content/plugins/bbpress/includes/admin/css/admin.css??2.5.3-5249"
      , "/wp-content/plugins/bbpress/templates/default/css/bbpress.css??2.5.3-5249"
      , "/wp-content/plugins/bbpress/templates/default/js/editor.js??2.5.3-5249"
      , "/wp-content/plugins/buddypress/bp-core/admin/css/common.min.css??2.0.1"
      , "/wp-content/plugins/buddypress/bp-core/css/admin-bar.min.css??2.0.1"
      , "/wp-content/plugins/buddypress/bp-core/js/confirm.min.js??2.0.1"
      , "/wp-content/plugins/buddypress/bp-templates/bp-legacy/css/buddypress.css??2.0.1"
      , "/wp-content/plugins/buddypress/bp-templates/bp-legacy/js/buddypress.js??2.0.1"
      , "/wp-content/themes/twentytwelve/js/navigation.js??20140318"
      , "/wp-content/themes/twentytwelve/style.css??3.9.1"
      , "/wp-includes/css/admin-bar.min.css??3.9.1"
      , "/wp-includes/css/buttons.min.css??3.9.1"
      , "/wp-includes/css/dashicons.min.css??3.9.1"
      , "/wp-includes/css/editor.min.css??3.9.1"
      , "/wp-includes/js/admin-bar.min.js??3.9.1"
      , "/wp-includes/js/comment-reply.min.js??3.9.1"
      , "/wp-includes/js/jquery/jquery-migrate.min.js??1.2.1"
      , "/wp-includes/js/jquery/jquery.js??1.11.0"
      , "/wp-includes/js/thickbox/loadingAnimation.gif?"
      , "/wp-includes/js/thickbox/thickbox.css??3.9.1"
      ]
    ces =
      Set.fromList . map (CacheEntry "") $
      map (CacheKey (DomainName "google.com")) paths
    cases =
      [ ("/(.*)", Set.size ces)
      , ("/", 1)
      , ("/wp-content/(.*)", 13)
      , ("/foo/", 3)
      ]
