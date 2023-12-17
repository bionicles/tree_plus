const routes: Routes = [
  { path: '', component: HomeComponent },
  {
    path: 'heroes',
    component: HeroesListComponent,
    children: [
      { path: ':id', component: HeroDetailComponent },
      { path: 'new', component: HeroFormComponent },
    ],
  },
  { path: '**', component: PageNotFoundComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})