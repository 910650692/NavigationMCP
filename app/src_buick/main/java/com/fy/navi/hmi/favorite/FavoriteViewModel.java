package com.sgm.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

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
}

