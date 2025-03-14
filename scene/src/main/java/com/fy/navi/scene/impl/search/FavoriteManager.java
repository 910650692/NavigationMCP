package com.fy.navi.scene.impl.search;

import com.android.utils.ToastUtils;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.Date;

/**
 * @Author: baipeng0904
 * @Description: FavoriteManager
 * @CreateDate: 2025/2/28 18:51
 */
public class FavoriteManager {
    private FavoriteManager() {
    }

    private static class SingletonHelper {
        private static final FavoriteManager INSTANCE = new FavoriteManager();
    }

    public static FavoriteManager getInstance() {
        return SingletonHelper.INSTANCE;
    }

    /**
     * 添加常去地址
     * int COLLECTION = 0; // 收藏
     * int HOME = 1; // 家
     * int COMPANY = 2; // 公司
     * int COMMON = 3; //常用地址
     */
    public void addFavorite(PoiInfoEntity poiInfoEntity, int type) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            return;
        }
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(type)
                .setItemId(poiInfoEntity.getPid()
                        + "_" + poiInfoEntity.getName()
                        + "_" + poiInfoEntity.getPoint().getLon()
                        + "_" + poiInfoEntity.getPoint().getLat()).setUpdateTime(new Date().getTime());
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavoriteData(poiInfoEntity, type);
        SettingUpdateObservable.getInstance().onUpdateSyncTime();
        ToastUtils.Companion.getInstance().showCustomToastView("已添加");
    }
}
