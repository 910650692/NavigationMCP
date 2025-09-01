package com.sgm.navi.hmi.favorite;

import android.app.Application;
import android.view.View;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.HashMap;
import java.util.Map;


public class FavoriteViewModel extends BaseFavoriteViewModel {

    public FavoriteViewModel(final @NonNull Application application) {
        super(application);
    }


    /**
     * 不同车型获取不同数据（buick）
     * @return map
     */
    public Map<String, Integer> getPopupData() {
        return new HashMap<>(){{
            put("homeOfficeY", -95);
            put("frequentY", -130);
            put("addButtonMargin", 20);
        }};
    }

    /**
     * ND打开新的fragment, 其他车型打开popup
     * @param view
     */
    public void showRenameDialog(final PoiInfoEntity poiInfo, final View view) {
        if(!ConvertUtils.isNull(mView)){
            mView.openFavoriteRenameFragment(poiInfo);
        }
    }
}

