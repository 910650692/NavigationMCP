package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneNaviChargeListView;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
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
public class SceneChargeListViewImpl extends BaseSceneModel<SceneNaviChargeListView> implements ILayerPackageCallBack {
    private static final String TAG = "SceneChargeListViewImpl";
    private final SearchPackage mSearchPackage;
    private final NaviPackage mNaviPackage;
    private final LayerPackage mLayerPackage;
    public SceneChargeListViewImpl(SceneNaviChargeListView screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mLayerPackage.registerCallBack(mMapTypeId, this);
    }

    @Override
    protected void onDestroy() {
        mLayerPackage.unRegisterCallBack(mMapTypeId, this);
        super.onDestroy();
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
        Logger.i(TAG, "showPreview start!", "selectIndex:" + mScreenView.getSelectIndex());
        if (ConvertUtils.isEmpty(list) || list.size() <= mScreenView.getSelectIndex()) {
            Logger.e(TAG, "showPreview failed!");
            return;
        }
        ThreadManager.getInstance().execute(() -> {
            mSearchPackage.createPoiMarker(list, mScreenView.getSelectIndex());
            OpenApiHelper.enterPreview(mMapTypeId);
        });
    }

    @Override
    public void onSearchItemClick(MapType mapTypeId, LayerItemSearchResult clickResult) {
        ILayerPackageCallBack.super.onSearchItemClick(mapTypeId, clickResult);
        //TODO 暂时点击不了，待测试和修改
        Logger.i(TAG, "onSearchItemClick:" + (clickResult == null ? "null" : clickResult.getStrID()));
    }
}
