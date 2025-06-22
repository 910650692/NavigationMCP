package com.sgm.navi.hmi.favorite;

import android.app.Application;

import androidx.annotation.NonNull;

import java.util.HashMap;
import java.util.Map;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/23
 */
public class FavoriteViewModel extends BaseFavoriteViewModel {

    public FavoriteViewModel(@NonNull Application application) {
        super(application);
    }

    /**
     * 不同车型获取不同数据(cadillac)
     * @return
     */
    public Map<String, Integer> getPopupData() {
        return new HashMap<>(){{
            put("homeOfficeY", -120);
            put("frequentY", -165);
            put("addButtonMargin", 32);
        }};
    }
}

