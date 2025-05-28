package com.fy.navi.hmi.favorite;

import android.app.Application;
import android.view.View;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.search.PoiInfoEntity;


public class FavoriteViewModel extends BaseFavoriteViewModel {

    public FavoriteViewModel(final @NonNull Application application) {
        super(application);
    }


    /**
     * ND打开新的fragment, 其他车型打开popup
     * @param view
     */
    public void showRenameDialog(final PoiInfoEntity poiInfo, final View view) {
        mView.openFavoriteRenameFragment(poiInfo);
    }
}

