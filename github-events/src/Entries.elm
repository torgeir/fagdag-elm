module Entries exposing (..)

import Html exposing (img, a, div, text, span)
import Html.Attributes exposing (href, src, height, width)
import Github


type alias Model =
    List Entry


type alias Entry =
    { user : Profile
    , action : Github.EventAction
    , avatar : String
    , timestamp : String
    , repo : Repo
    }


type alias Repo =
    { name : String
    , url : String
    }


type alias Profile =
    { name : String
    , url : String
    }


type alias Username =
    String


type alias Usernames =
    List Username


view : Entry -> Html.Html t
view entry =
    div []
        [ img [ src entry.avatar, height 20, width 20 ] []
        , text " "
        , a [ href entry.user.url ] [ text entry.user.name ]
        , text " "
        , description entry
        ]


pullView : Github.PullRequest -> Html.Html t
pullView pr =
    a [ href pr.url ]
        [ text
            ("pull request #"
                ++ (toString pr.number)
                ++ " - "
                ++ pr.title
            )
        ]


description : Entry -> Html.Html t
description entry =
    let
        repo =
            repoView entry.repo
    in
        case entry.action of
            Github.EventActionPullRequestReviewComment comment pr ->
                span []
                    [ a [ href comment.url ] [ text "placed a review comment" ]
                    , text " in "
                    , pullView pr
                    ]

            Github.EventActionPullRequest pr ->
                span []
                    [ text (pr.action ++ " ")
                    , pullView pr
                    , text " in "
                    , repo
                    ]

            Github.EventActionPush _ ->
                span []
                    [ text "pushed to ", repo ]

            Github.EventActionIssue issueAction ->
                span []
                    [ text (issueAction.action ++ " ")
                    , a [ href issueAction.issue.url ]
                        [ text
                            ("issue #"
                                ++ (toString issueAction.issue.number)
                                ++ " - "
                                ++ issueAction.issue.title
                            )
                        ]
                    , text " in "
                    , repo
                    ]

            Github.EventActionIssueComment issueComment ->
                span []
                    [ a [ href issueComment.comment.url ] [ text "commented" ]
                    , text " on "
                    , a [ href issueComment.issue.url ]
                        [ text
                            ("issue #"
                                ++ (toString issueComment.issue.number)
                            )
                        ]
                    , text " in "
                    , repo
                    ]

            Github.EventActionCommitComment comment ->
                span []
                    [ a [ href comment.url ] [ text "commented" ]
                    , text " on a commit in "
                    , repo
                    ]

            Github.EventActionFork ->
                span [] [ text "forked ", repo ]

            Github.EventActionWatch ->
                span [] [ text "starred ", repo ]

            Github.EventActionMember user action ->
                span []
                    [ text user
                    , text " "
                    , text action
                    , text " to "
                    , repo
                    ]

            Github.EventActionCreate ref refType description ->
                case refType of
                    "repository" ->
                        span []
                            [ text ("created the " ++ refType ++ " ")
                            , repo
                            , text
                                (Maybe.withDefault " "
                                    (Maybe.map
                                        (\d -> " - " ++ d ++ " ")
                                        description
                                    )
                                )
                            ]

                    "branch" ->
                        span []
                            [ text
                                ("created a "
                                    ++ refType
                                    ++ " in "
                                )
                            , repo
                            ]

                    _ ->
                        text ""

            Github.EventActionUnknown type' ->
                span []
                    [ text ("<unhandled event type " ++ type' ++ "> ")
                    , repo
                    ]


repoView : Repo -> Html.Html t
repoView repo =
    a [ href repo.url ] [ text repo.name ]


joinEntries : Model -> Model -> Model
joinEntries entries moreEntries =
    entries
        |> List.append moreEntries
        |> List.sortBy .timestamp
        |> List.reverse
