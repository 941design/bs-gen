module Testimonial (testimonial) where

import Test.QuickCheck.Gen

-- https://blog.hubspot.com/marketing/testimonial-page-examples#sm.0000126iyiq2p6dxtvgw5la1hbhvc

testimonial = frequency [ (3, action <++> space <++> term <++> space <++> result)
                        , (1, temporal <++> pure ", I was " <++> action <++> space <++> term)
                        , (2, pure "I was " <++> action <++> space <++> term <++> pure ", and it " <++> result)
                        , (2, term <++> space <++> prescription)
                        , (3, term <++> pure " are " <++> attribute)
                        ]

action = elements [ "adopting"
                  , "applying"
                  , "appreciating"
                  , "catching a glimpse of"
                  , "concentrating on"
                  , "employing"
                  , "establishing"
                  , "focusing on"
                  , "getting to know"
                  , "implementing"
                  , "investing into"
                  , "learning about"
                  , "prioritizing on"
                  , "studying"
                  , "working with"
                  , "zeroing in on"
                  ]

result = frequency [ (1, pure "transformed both my professional, as well as my private life")
                   , (1, pure "made my job worthwhile")
                   , (1, pure "started the transition we were looking for")
                   , (5, pure "had a " <++> adj <++> pure " impact on our business")
                   , (5, pure "impressed our " <++> whom <++> pure " like no other technology")
                   , (1, pure "saved our business")
                   , (1, pure "fostered public interest in our company")
                   , (2, pure "rejuvenated the relationship with our " <++> whom)
                   , (1, pure "raised our revenues by over " <++> number <++> pure "0 percent")
                   , (1, pure "helped us reach all our business goals")
                   , (1, pure "changed my thinking")
                   , (10, pure "was " <++> attribute)
                   ]
  where
    whom = elements [ "business partners"
                    , "customers"
                    , "investors"
                    , "shareholders"
                    ]
    adj = elements [ "enormous"
                   , "huge"
                   , "impressive"
                   , "magnificient"
                   , "tremendeous"
                   , "unbelievable"
                   , "unprecedented"
                   ]

prescription = frequency [ (1, pure "should be part of any C.S. curriculum")
                         , (1, pure "should be taught in elementary school")
                         , (1, pure "are an opportunity everyone should take")
                         , (1, pure "give you so many more options")
                         , (1, pure "can no longer be ignored")
                         , (1, pure "are not going to stop us") -- lol
                         , (1, pure "is just another definition of success")
                         , (6, pure "are " <++> attribute)
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
                  , frequency [ (1, adverb <++> space)
                              , (2, empty)
                              ] <++> elements [ "magnificient"
                                              , "inspiring"
                                              , "awesome"
                                              , "amazing"
                                              , "empowering"
                                              , "wonderful"
                                              , "easy to master"
                                              ]
                  ]
  where
    adverb = elements [ "genuinely"
                      , "incredibly"
                      , "just"
                      , "overwhelmingly"
                      , "really"
                      , "simply"
                      , "truly"                   
                      ]
      
temporal = oneof [ pure "for the last " <++> number <++> space <++> timeUnit
                 , pure "every " <++> number <++> space <++> timeUnit
                 , pure "in a few " <++> timeUnit
                 , pure "every day"
                 , pure "every morning"
                 , pure "on mondays"
                 , pure "on my coffee breaks"
                 , pure "at lunchtime"
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

empty = pure ""

space = pure " "

number = elements $ map show [2..10]

plural = (==) 's' . last

adverb = elements [ "completely"
                  , "continuously"
                  , "eventually"
                  , "extremely"
                  , "immediately"
                  , "independently"
                  , "modularly"
                  , "next-generation"
                  , "reliably"
                  , "seamlessly"
                  , "transparently"
                  , "truly"
                  ]

adjective = elements [ "actor-based"
                     , "agile"
                     , "autonomous"
                     , "certified"
                     , "configurable"
                     , "correlated"
                     , "dependency-aware"
                     , "effective"
                     , "external"
                     , "feature-driven"
                     , "fine grained"
                     , "fine granular"
                     , "highly available"
                     , "improved"
                     , "innovative"
                     , "integrated"
                     , "intuitive"
                     , "lightweight"
                     , "modular"
                     , "non-blocking"
                     , "parallel"
                     , "persistent"
                     , "radical"
                     , "reactive"
                     , "redundant"
                     , "resiliant"
                     , "scalable"
                     , "stateless"
                     , "technical"
                     ]

noun = noun1 <++> space <++> noun2
  where
    noun1 = elements [ "activator"
                     , "application"
                     , "business"
                     , "client"
                     , "cloud"
                     , "consumer"
                     , "core"
                     , "dynamic"
                     , "enterprise"
                     , "full-stack"
                     , "hypervisor"
                     , "key"
                     , "marketing"
                     , "meta"
                     , "process"
                     , "quality"
                     , "virtual"
                     ]
    noun2 = elements [ "architectures"
                     , "clusters"
                     , "distributions"
                     , "engines"
                     , "frameworks"
                     , "infrastructures"                     
                     , "interfaces"
                     , "metrics"
                     , "milestones"
                     , "networks"
                     , "platforms"
                     , "redesigns"
                     , "resources"
                     , "systems"
                     , "toolchains"
                     ]

term = frequency [ (1, adverb <++> space )
                 , (1, empty)
                 ] <++> adjective <++> space <++> noun

(<++>) :: Monoid a => Gen a -> Gen a -> Gen a
(<++>) a b = mappend <$> a <*> b
