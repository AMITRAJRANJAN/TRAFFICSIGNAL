VERSION 5.00
Begin VB.Form TRAFFICSIGNAL 
   BackColor       =   &H00000000&
   Caption         =   "TRAFFIC SIGNAL"
   ClientHeight    =   10440
   ClientLeft      =   120
   ClientTop       =   330
   ClientWidth     =   19005
   LinkTopic       =   "Form1"
   ScaleHeight     =   10440
   ScaleWidth      =   19005
   Begin VB.PictureBox SOUTH 
      BackColor       =   &H80000009&
      Height          =   6975
      Left            =   14640
      ScaleHeight     =   6915
      ScaleWidth      =   4035
      TabIndex        =   6
      Top             =   1680
      Width           =   4095
      Begin VB.Shape SGREEN 
         BackColor       =   &H0000C000&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   4680
         Width           =   2415
      End
      Begin VB.Shape SYELLOW 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   2520
         Width           =   2415
      End
      Begin VB.Shape SRED 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   240
         Width           =   2415
      End
   End
   Begin VB.PictureBox WEST 
      BackColor       =   &H80000009&
      Height          =   6975
      Left            =   9600
      ScaleHeight     =   6915
      ScaleWidth      =   4035
      TabIndex        =   5
      Top             =   1680
      Width           =   4095
      Begin VB.Shape WGREEN 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   4680
         Width           =   2415
      End
      Begin VB.Shape WYELLOW 
         BackColor       =   &H0000FFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   2520
         Width           =   2415
      End
      Begin VB.Shape WRED 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   240
         Width           =   2415
      End
   End
   Begin VB.PictureBox EAST 
      BackColor       =   &H80000009&
      Height          =   6975
      Left            =   4680
      ScaleHeight     =   6915
      ScaleWidth      =   4155
      TabIndex        =   2
      Top             =   1680
      Width           =   4215
      Begin VB.Shape ERED 
         BackColor       =   &H000000FF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   240
         Width           =   2415
      End
      Begin VB.Shape EYELLOW 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   2520
         Width           =   2415
      End
      Begin VB.Shape EGREEN 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   4680
         Width           =   2415
      End
   End
   Begin VB.Timer Timer1 
      Interval        =   1000
      Left            =   120
      Top             =   120
   End
   Begin VB.PictureBox NORTH 
      BackColor       =   &H80000009&
      Height          =   6975
      Left            =   240
      ScaleHeight     =   6915
      ScaleWidth      =   3915
      TabIndex        =   0
      Top             =   1680
      Width           =   3975
      Begin VB.Shape NGREEN 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   4680
         Width           =   2415
      End
      Begin VB.Shape NYELLOW 
         BackColor       =   &H00FFFFFF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   2520
         Width           =   2415
      End
      Begin VB.Shape NRED 
         BackColor       =   &H000000FF&
         BackStyle       =   1  'Opaque
         Height          =   1935
         Left            =   840
         Shape           =   2  'Oval
         Top             =   240
         Width           =   2415
      End
   End
   Begin VB.Label SSECOND 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   54.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1335
      Left            =   15000
      TabIndex        =   11
      Top             =   240
      Width           =   3135
   End
   Begin VB.Label ESECOND 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   54.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1335
      Left            =   9960
      TabIndex        =   10
      Top             =   240
      Width           =   3135
   End
   Begin VB.Label WSECOND 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   54.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1335
      Left            =   5160
      TabIndex        =   9
      Top             =   240
      Width           =   3135
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      Caption         =   "SOUTH"
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1095
      Left            =   15600
      TabIndex        =   8
      Top             =   8880
      Width           =   2535
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      Caption         =   "WEST"
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1095
      Left            =   10560
      TabIndex        =   7
      Top             =   8880
      Width           =   2535
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      Caption         =   "EAST"
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1095
      Left            =   5520
      TabIndex        =   4
      Top             =   8880
      Width           =   2535
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      Caption         =   "NORTH"
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   20.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1095
      Left            =   240
      TabIndex        =   3
      Top             =   8880
      Width           =   2895
   End
   Begin VB.Label NSECOND 
      Alignment       =   2  'Center
      BackColor       =   &H80000007&
      BeginProperty Font 
         Name            =   "MS Reference Sans Serif"
         Size            =   54.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   1335
      Left            =   720
      TabIndex        =   1
      Top             =   240
      Width           =   3135
   End
End
Attribute VB_Name = "TRAFFICSIGNAL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim SS%


Private Sub Timer1_Timer()
L1:    SS = SS + 1
    NSECOND.Caption = SS
    WSECOND.Caption = SS
    ESECOND.Caption = SS
    SSECOND.Caption = SS
    If SS = 15 Then
    SRED.BackColor = &HFF&
    SGREEN.BackColor = &HFFFFFF
    SYELLOW.BackColor = &HFFFFFF
    NYELLOW.BackColor = &HFFFF&
    NRED.BackColor = &HFFFFFF
    NGREEN.BackColor = &HFFFFFF
    WGREEN.BackColor = &HFF00&
    WYELLOW.BackColor = &HFFFFFF
    WRED.BackColor = &HFFFFFF
    ERED.BackColor = &HFF&
    EYELLOW.BackColor = &HFFFFFF
    EGREEN.BackColor = &HFFFFFF
    End If
    
    If SS = 30 Then
    SRED.BackColor = &HFF&
    SGREEN.BackColor = &HFFFFFF
    SYELLOW.BackColor = &HFFFFFF
    NYELLOW.BackColor = &HFFFFFF
    NRED.BackColor = &HFFFFFF
    NGREEN.BackColor = &HFF00&
    WGREEN.BackColor = &HFFFFFF
    WYELLOW.BackColor = &HFFFFFF
    WRED.BackColor = &HFF&
    ERED.BackColor = &HFFFFFF
    EYELLOW.BackColor = &HFFFF&
    EGREEN.BackColor = &HFFFFFF
    End If
    
    If SS = 45 Then
    SRED.BackColor = &HFFFFFF
    SGREEN.BackColor = &HFFFFFF
    SYELLOW.BackColor = &HFFFF&
    NYELLOW.BackColor = &HFFFFFF
    NRED.BackColor = &HFF&
    NGREEN.BackColor = &HFFFFFF
    WGREEN.BackColor = &HFFFFFF
    WYELLOW.BackColor = &HFFFFFF
    WRED.BackColor = &HFF&
    ERED.BackColor = &HFFFFFF
    EYELLOW.BackColor = &HFFFFFF
    EGREEN.BackColor = &HFF00&
    End If
             
    If SS = 60 Then
                SRED.BackColor = &HFFFFFF
                SGREEN.BackColor = &HFF00&
                SYELLOW.BackColor = &HFFFFFF
                NYELLOW.BackColor = &HFFFFFF
                NRED.BackColor = &HFF&
                NGREEN.BackColor = &HFFFFFF
                WGREEN.BackColor = &HFFFFFF
                WYELLOW.BackColor = &HFFFF&
                WRED.BackColor = &HFFFFFF
                ERED.BackColor = &HFF&
                EYELLOW.BackColor = &HFFFFFF
                EGREEN.BackColor = &HFFFFFF
                SS = 0
            GoTo L1
    End If
End Sub

