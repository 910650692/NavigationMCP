package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.hangingcard.NaviSceneHandingCardDetail;
import com.fy.navi.scene.ui.navi.hangingcard.OnHandingCardItemClickListener;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.scene.util.HandCardType;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/19
 * Description: [悬挂卡列表详情]
 */
public class NaviSceneHandingCardDetailImpl extends BaseSceneModel<NaviSceneHandingCardDetail> implements OnHandingCardItemClickListener, IRouteResultObserver, ImmersiveStatusScene.IImmersiveStatusCallBack {
    private List<PoiInfoEntity> mList = new ArrayList<>();
    private HandCardType mType = HandCardType.CHARGE;
    private RoutePackage mRoutePackage;
    private SearchPackage mSearchPackage;
    private LayerPackage mLayerPackage;
    private MapPackage mapPackage;
    private long requestId;
    private final String TAG = "NaviSceneHandingCardDetailImpl";

    public NaviSceneHandingCardDetailImpl(NaviSceneHandingCardDetail screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
        mRoutePackage.registerRouteObserver(TAG, this);
        mSearchPackage = SearchPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mapPackage = MapPackage.getInstance();
        ImmersiveStatusScene.getInstance().registerCallback(TAG, this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mRoutePackage.unRegisterRouteObserver(TAG);
        ImmersiveStatusScene.getInstance().unRegisterCallback(TAG);
    }

    @Override
    public void onItemSelect(int position) {
        // TODO 选中
    }

    @Override
    public void onNaviNow(int position) {
        if (!ConvertUtils.isNull(mCallBack)) {
            requestId = mRoutePackage.requestChangeEnd(mMapTypeId, mList.get(position));
        }
    }

    @Override
    public void onRouteSuccess(String successMsg) {
        IRouteResultObserver.super.onRouteSuccess(successMsg);
        Map<MapType, Long> ids = mRoutePackage.getRequestIds();
        if (!ConvertUtils.isEmpty(ids) && ids.containsValue(requestId)) {
            mScreenView.notifySceneStateChange(false);
            NaviSceneManager.getInstance().notifySceneStateChange(
                    INaviSceneEvent.SceneStateChangeType.SceneCloseState,
                    NaviSceneId.NAVI_SUSPEND_CARD
            );
        }
    }

    public void updateUi(List<PoiInfoEntity> list, HandCardType type) {
        this.mList.clear();
        this.mList.addAll(list);
        this.mType = type;
    }

    /**
     * 显示停车场扎标并且选定推荐的停车场
     *
     * @param select select
     */
    public void showPreview(final int select) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.TOUCH);
        Logger.i(TAG, "showPreview:" + select);
        ThreadManager.getInstance().execute(() -> mSearchPackage.createPoiMarker(mList, select));
        mLayerPackage.setFollowMode(mMapTypeId, false);
        mLayerPackage.setDynamicLevelLock(mMapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
        final PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(getParkingBound());
        mapPackage.showPreview(mMapTypeId, previewParams);
    }

    public void exitPreview() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        mLayerPackage.setFollowMode(mMapTypeId, true);
        mSearchPackage.clearLabelMark();
        mLayerPackage.setDynamicLevelLock(mMapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, false);
        mapPackage.exitPreview(mMapTypeId);
        Logger.i(TAG, "exitPreview success!");
    }

    /**
     * @return RectDouble
     */
    public PreviewParams.RectDouble getParkingBound() {
        // TODO: 2025/2/17 还需当前定位点
//        NeLocation location = LocationManager.getInstance().getLastLocation();
//        BizPointBusinessInfo info = new BizPointBusinessInfo();
//        info.mPos3D.lon = location.lon;
//        info.mPos3D.lat = location.lat;
//        pois.add(info);
        try {
            double x1 = Double.MAX_VALUE;
            double y1 = Double.MAX_VALUE;
            double x2 = Double.MIN_VALUE;
            double y2 = Double.MIN_VALUE;
            for (int i = 0; i < mList.size(); i++) {
                final PoiInfoEntity oItem = mList.get(i);
                x1 = Math.min(x1, oItem.getPoint().getLon());
                y1 = Math.min(y1, oItem.getPoint().getLat());
                x2 = Math.max(x2, oItem.getPoint().getLon());
                y2 = Math.max(y2, oItem.getPoint().getLat());
            }
            return new PreviewParams.RectDouble(x1, x2, y2, y1);
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus lastImersiveStatus) {
        if (lastImersiveStatus == ImersiveStatus.IMERSIVE && mScreenView.isVisible()) {
            mScreenView.notifySceneStateChange(false);
            Logger.i(TAG, "onImmersiveStatusChange-close-self!");
        }
    }
}
