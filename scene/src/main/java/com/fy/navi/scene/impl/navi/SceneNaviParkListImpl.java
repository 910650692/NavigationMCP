package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.INaviParkItemClickListener;
import com.fy.navi.scene.api.navi.ISceneNaviParkList;
import com.fy.navi.scene.ui.navi.SceneNaviParkListView;
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;

import java.util.ArrayList;
import java.util.concurrent.ScheduledFuture;


public class SceneNaviParkListImpl extends BaseSceneModel<SceneNaviParkListView> implements
        ISceneNaviParkList, INaviParkItemClickListener, ILayerPackageCallBack {
    private static final String TAG = "SceneNaviParkListImpl";

    private int mTimes = NumberUtils.NUM_8;

    private ScheduledFuture mScheduledFuture;
    private ArrayList<NaviParkingEntity> mParkingList = new ArrayList<>();
    private MapPackage mMapPackage;
    private LayerPackage mLayerPackage;
    private SearchPackage mSearchPackage;
    private RoutePackage mRoutePackage;
    private NaviPackage mNaviPackage;

    public SceneNaviParkListImpl(final SceneNaviParkListView screenView) {
        super(screenView);
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
    }

    @Override
    protected void onCreate() {
        mLayerPackage.registerCallBack(mMapTypeId, this);
    }

    @Override
    protected void onDestroy() {
        mLayerPackage.clearSearchParkPoi(mMapTypeId);
        mSearchPackage.unRegisterCallBack("SceneNaviParkListImpl");
        mLayerPackage.unRegisterCallBack(mMapTypeId, this);
        super.onDestroy();
    }

    @Override
    public void closeParkList() {
        mLayerPackage.clearSearchParkPoi(mMapTypeId);
    }

    /**
     * 显示停车场扎标并且选定推荐的停车场
     *
     * @param select select
     */
    public void showPreview(final int select) {
        Logger.i(TAG, "showPreview:" + select);
        //TODO 后续图层修改后这边的逻辑可能要修改
        mLayerPackage.updateSearchParkPoi(mMapTypeId, mParkingList);
        final PreviewParams previewParams = new PreviewParams();
        previewParams.setMapBound(getParkingBound(mParkingList));
        mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(select), true);
        mLayerPackage.setFollowMode(mMapTypeId, false);
        mMapPackage.showPreview(mMapTypeId, previewParams);
    }

    /**
     * @param pois pois
     * @return RectDouble
     */
    public static PreviewParams.RectDouble getParkingBound(
            final ArrayList<NaviParkingEntity> pois) {
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
            for (int i = 0; i < pois.size(); i++) {
                final NaviParkingEntity oItem = pois.get(i);
                x1 = Math.min(x1, oItem.getPoint().getLon());
                y1 = Math.min(y1, oItem.getPoint().getLat());
                x2 = Math.max(x2, oItem.getPoint().getLon());
                y2 = Math.max(y2, oItem.getPoint().getLat());
            }
            return new PreviewParams.RectDouble(x1, x2, y2, y1);
        } catch (Exception e) {
            Logger.e(TAG, "getParkingBound error" + e.getMessage());
            return null;
        }
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_DESTINATION_PARKING_SELECT)
    public void onItemClick(final int position) {
        if (!ConvertUtils.isNull(mLayerPackage)) {
            mLayerPackage.setParkFocus(mMapTypeId, String.valueOf(position), true);
        }

        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, Integer.toString(position + 1))
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }

    @Override
    public void onNaviClick(final int position, final NaviParkingEntity entity) {
        Logger.i(TAG, "position：" + position + ",entity：" + entity.getName());
        mRoutePackage.requestChangeEnd(mMapTypeId, NaviDataFormatHelper.getPoiInfoEntity(entity));
        if (!ConvertUtils.isNull(mScreenView)) {
            mScreenView.notifySceneStateChange(false);
        }
    }

    @Override
    public void onNotifyClick(final MapType mapTypeId, final GemBaseLayer layer,
                              final GemLayerItem item) {
        if (item == null) {
            Logger.e(TAG, "pItem == null");
            return;
        }
        final int index = (int) item.getIndex();
        Logger.i(TAG, "position：" + index);
        ThreadManager.getInstance().postUi(() -> mScreenView.onItemClick(index));
    }

    public void exitPreview() {
        Logger.i(TAG, "exitPreview");
        if (!ConvertUtils.isNull(mNaviPackage)) {
            mNaviPackage.setPreviewStatus(false);
        }
        if (!ConvertUtils.isNull(mMapPackage)) {
            mMapPackage.exitPreview(mMapTypeId);
        }
        if (!ConvertUtils.isNull(mLayerPackage)) {
            mLayerPackage.clearSearchParkPoi(mMapTypeId);
            mLayerPackage.setFollowMode(mMapTypeId, true);
        }
    }
}
