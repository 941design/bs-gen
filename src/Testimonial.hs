module Testimonial (testimonial) where

import Test.QuickCheck.Gen

-- https://blog.hubspot.com/marketing/testimonial-page-examples#sm.0000126iyiq2p6dxtvgw5la1hbhvc

testimonial = oneof [ action <++> space <++> term <++> space <++> result
                    , temporal <++> pure ", I was " <++> action <++> space <++> term
                    , pure "I was " <++> action <++> space <++> term <++> pure ", and it " <++> result
                    , term <++> space <++> prescription
                    , term <++> pure " are " <++> attribute
                    ]
  
action = elements [ "working with"
                  , "implementing"
                  , "adopting"
                  , "studying"
                  , "getting to know"
                  , "learning about"
                  , "concentrating on"
                  , "catching a glimpse of"
                  , "appreciating"
                  ]

result = oneof [ pure "transformed both my professional, as well as my private life"
               , pure "made my job worthwhile"
               , pure "started the transition we were looking for"
               , pure "had a " <++> adj <++> pure " impact on our business"
               , pure "impressed our " <++> whom <++> pure " like no other technology"
               , pure "raised our revenues by over " <++> number <++> pure "0 percent"
               , pure "helped us reach all our business goals"
               , pure "changed my thinking"
               , pure "was " <++> attribute
               ]
  where
    whom = elements [ "business partners"
                    , "shareholders"
                    , "customers"
                    ]
    adj = elements [ "enormous"
                   , "huge"
                   , "tremendeous"
                   , "impressive"
                   , "unprecedented"
                   ]

prescription = oneof [ pure "should be part of any C.S. curriculum"
                     , pure "should be taught in elementary school"
                     , pure "are an opportunity everyone should take"
                     , pure "give you so many more options"                       
                     , pure "can no longer be ignored"
                     , pure "are not going to stop us"
                     , pure "is just another definition of success"                                              
                     , pure "are " <++> attribute
                     ]

attribute = oneof [ pure "an experience I don't want to miss"
                  , pure "intimidating at first, but definitely worth the ride"
                  , pure "incredible"
                  , pure "a game changer"
                  , pure "quite something"
                  , pure "something anyone can learn"                    
                  , pure "a true privilege"
                  , pure "a boost for my career"
                  , pure "the next big thing"
                  , pure "my greatest accomplishment"                    
                  , pure "exactly what I was looking for"
                  , adverb <++> elements [ " magnificient"
                                         , " inspiring"
                                         , " awesome"
                                         , " amazing"
                                         , " empowering"
                                         , " wonderful"
                                         , " easy to master"
                                         ]
                  ]
  where
    adverb = elements [ "truly"
                      , "genuinely"
                      , "overwhelmingly"
                      , "really"
                      , "incredibly"
                      , "just"
                      ]
      
temporal = oneof [ pure "for the last " <++> number <++> space <++> timeUnit
                 , pure "every " <++> number <++> space <++> timeUnit
                 , pure "in a few " <++> timeUnit
                 , pure "many " <++> timeUnit
                 , pure "for some " <++> timeUnit
                 , pure "every day"
                 , pure "every morning"
                 , pure "on mondays"
                 , pure "on my coffee breaks"
                 , pure "at lunchtime"
                 , pure "regularly"
                 , pure "often"
                 , pure "continuously"
                 ]

timeUnit = elements [ "decades"
                    , "years"
                    , "months"
                    , "weeks"
                    , "days"
                    , "hours"
                    , "minutes"
                    , "seconds"
                    , "milliseconds"
                    , "fractions of a second"
                    ]

space = pure " "

number = elements $ map show [2..10]

plural = (==) 's' . last

adverb = elements [ "extremely"
                  , "continuously"
                  , "completely"
                  , "eventually"
                  , "seamlessly"
                  , "immediately"
                  , "transparently"
                  , "independently"
                  , "reliably"
                  , "modularly"
                  , "next-generation"
                  , "truly"
                  ]

adjective = elements [ "reactive"
                     , "resiliant"
                     , "intuitive"
                     , "scalable"
                     , "effective"
                     , "stateless"
                     , "dependency-aware"
                     , "non-blocking"
                     , "redundant"
                     , "correlated"
                     , "agile"
                     , "improved"
                     , "integrated"
                     , "technical"
                     , "lightweight"
                     , "certified"
                     , "external"
                     , "feature-driven"
                     , "highly available"
                     , "fine granular"
                     , "actor-based"
                     , "configurable"
                     , "parallel"
                     , "persistent"
                     , "radical"
                     ]

noun = noun1 <++> space <++> noun2
  where
    noun1 = elements [ "meta"
                     , "hypervisor"
                     , "activator"
                     , "client"
                     , "process"
                     , "consumer"
                     , "dynamic"
                     , "enterprise"
                     , "application"
                     , "quality"
                     , "cloud"
                     , "full-stack"
                     , "core"
                     , "business"
                     , "virtual"
                     , "marketing"
                     , "key"
                     ]
    noun2 = elements [ "infrastructures"
                     , "networks"
                     , "architectures"
                     , "clusters"
                     , "engines"
                     , "frameworks"
                     , "systems"
                     , "platforms"
                     , "metrics"
                     , "resources"
                     , "interfaces"
                     , "distributions"
                     , "toolchains"
                     , "redesigns"
                     , "milestones"
                     ]

term = adverb <++> space <++> adjective <++> space <++> noun

(<++>) :: Monoid a => Gen a -> Gen a -> Gen a
(<++>) a b = mappend <$> a <*> b

