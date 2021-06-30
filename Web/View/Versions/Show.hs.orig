module Web.View.Versions.Show where
import Web.View.Prelude

data ShowView = ShowView { version :: Version }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={VersionsAction}>Versions</a></li>
                <li class="breadcrumb-item active">Show Version</li>
            </ol>
        </nav>
        <h1>Show Version</h1>
        <p>{version}</p>
    |]
