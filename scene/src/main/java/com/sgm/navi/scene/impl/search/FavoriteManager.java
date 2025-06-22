package com.sgm.navi.scene.impl.search;

import com.android.utils.ToastUtils;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: FavoriteManager
 * @CreateDate: 2025/2/28 18:51
 */
final public class FavoriteManager {
    private static final String DIVIDER = "_";
    private FavoriteManager() {
    }

    private static final class SingletonHelper {
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
     * @param poiInfoEntity poiInfoEntity实体类
     * @param type 类型
     */
    public void addFavorite(final PoiInfoEntity poiInfoEntity, final int type) {
        if (poiInfoEntity == null || poiInfoEntity.getPoint() == null) {
            return;
        }
        if (BehaviorPackage.getInstance().isFavorite(poiInfoEntity.getPid()+type)) {
            return;
        }
        final FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setCommonName(type);
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        BehaviorPackage.getInstance().addFavorite(poiInfoEntity, type);
        SettingUpdateObservable.getInstance().onUpdateSyncTime();
        ToastUtils.Companion.getInstance().showCustomToastView("已添加");
    }
}
