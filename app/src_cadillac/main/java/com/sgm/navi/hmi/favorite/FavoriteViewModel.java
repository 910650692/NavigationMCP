package com.sgm.navi.hmi.favorite;

import android.app.Application;
import android.view.View;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.sgm.navi.service.define.search.PoiInfoEntity;

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

