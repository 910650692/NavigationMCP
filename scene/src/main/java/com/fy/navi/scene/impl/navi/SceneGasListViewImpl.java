package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneGasListView;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.List;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/2
 * Description: [在这里描述文件功能]
 */
public class SceneGasListViewImpl extends BaseSceneModel<SceneGasListView> {
    private static final String TAG = "SceneGasListViewImpl";
    private final SearchPackage mSearchPackage;
    private final NaviPackage mNaviPackage;

    public SceneGasListViewImpl(SceneGasListView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
    }

    /**
     * 导航继续
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_CONTINUE)
    public void naviContinue() {
        Logger.i(TAG, "naviContinue");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(
                mMapTypeId, ImersiveStatus.IMERSIVE);
        ThreadManager.getInstance().execute(() -> {
            if (!mNaviPackage.getFixedOverViewStatus()) {
                mSearchPackage.clearLabelMark();
                OpenApiHelper.exitPreview(mMapTypeId);
            }
        });
    }

    /***
     * 扎标并显示全览
     */
    public void showPreview(final List<PoiInfoEntity> list) {
        if (ConvertUtils.isEmpty(list) || list.size() <= mScreenView.getSelectIndex()) {
            Logger.e(TAG, "showPreview failed!");
            return;
        }
        ThreadManager.getInstance().execute(() -> {
            mSearchPackage.createPoiMarker(list, mScreenView.getSelectIndex());
            OpenApiHelper.enterPreview(mMapTypeId);
        });
    }
}
