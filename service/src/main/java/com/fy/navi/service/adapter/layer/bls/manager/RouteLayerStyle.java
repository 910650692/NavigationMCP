package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.RectDouble;
import com.autonavi.gbl.common.path.model.RestAreaInfo;
import com.autonavi.gbl.common.path.model.RoutePoints;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.model.BizPathInfoAttrs;
import com.autonavi.gbl.layer.model.BizRouteDrawCtrlAttrs;
import com.autonavi.gbl.layer.model.BizRouteMapMode;
import com.autonavi.gbl.layer.model.BizRouteRestAreaInfo;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizRouteWeatherInfo;
import com.autonavi.gbl.layer.model.DynamicLevelParam;
import com.autonavi.gbl.layer.model.RouteDrawStyle;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.RouteLayerScene;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.autonavi.gbl.route.model.WeatherLabelItem;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteLinePoints;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class RouteLayerStyle extends BaseLayerStyle {
    private static final java.lang.String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    private ArrayList<String> mEstimatedTimeOfArrival = new ArrayList<>();
    private String mCurrentRouteTime = "";

    public RouteLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
        mBziRouteControl.initDynamicLevel(assembleDynamicLevelParam());
    }

    public void addLayerClickListener() {
        mBziRouteControl.addClickObserver(this);
    }

    public void removeLayerClickListener() {
        mBziRouteControl.removeClickObserver(this);
    }

    public PreviewParams getPathResultBound(ArrayList<?> pathResult) {
        ArrayList<PathInfo> pathInfos = (ArrayList<PathInfo>) pathResult;
        RectDouble rectDouble = BizGuideRouteControl.getPathResultBound(pathInfos);
        Logger.d(TAG, "path info 转换为预览巨型区域参数：", rectDouble);
        PreviewParams previewParams = new PreviewParams();
        previewParams.setRouteLine(true);
        previewParams.setbUseRect(true);
        previewParams.setMapBound(new PreviewParams.RectDouble(rectDouble.left, rectDouble.right, rectDouble.top, rectDouble.bottom));
        return previewParams;
    }

    public void drawRoute(RouteLineLayerParam routeLineLayer) {
        ConvertUtils.isNullRequire(routeLineLayer, "路线绘制参数为空，无法进行路线渲染");
        mBziRouteControl.getRouteLayer(BizRouteType.BizRouteTypeTrafficEventTip).enableCollision(true);
        setPassGreyMode(routeLineLayer.isPassGrey());
        setMainMapPathDrawStyle();
        setPathPoints(routeLineLayer.getRouteLinePoints());
        mBziRouteControl.setVisible(BizRouteType.BizRouteTypeEnergyRemainPoint, true);
        setPathInfos(routeLineLayer.getPathInfoList(), routeLineLayer.getSelectIndex());
        if (routeLineLayer.getEstimatedTimeOfArrival() != null) {
            mEstimatedTimeOfArrival = routeLineLayer.getEstimatedTimeOfArrival();
        }
        updatePaths();
    }

    /**
     * 走过的路线置灰.
     *
     * @param bPassGrey 是否置灰
     */
    public void setPassGreyMode(boolean bPassGrey) {
        mBziRouteControl.setPassGreyMode(true);
    }

    /**
     * 设置选中路线.
     *
     * @param index 被选路线id
     */
    public void setSelectedPathIndex(int index) {
        if (mEstimatedTimeOfArrival != null && mEstimatedTimeOfArrival.size() > index) {
            mCurrentRouteTime = mEstimatedTimeOfArrival.get(index);
        } else {
            mCurrentRouteTime = "";
        }
        mBziRouteControl.setSelectedPathIndex(index);
        updatePaths();
    }

    /**
     * 获取预计到达时间.
     *
     */
    public String getCurrentRouteTime() {
        return mCurrentRouteTime;
    }

    /**
     * 获取选择的路线id.
     *
     * @return 路线id
     */
    public int getSelectedPathIndex() {
        return mBziRouteControl.getSelectedPathIndex();
    }

    /**
     * 清除路线以及缓存的路线数据(包括路线，路线上的点，箭头，交通事件tip，拥堵事件，封路事件等).
     */
    public void clearPaths() {
        mBziRouteControl.clearPaths();
    }

    /**
     * 设置样式风格.
     */
    private void setMainMapPathDrawStyle() {
        // 路线绘制风格
        RouteDrawStyle drawParam = new RouteDrawStyle();
        drawParam.mIsNavi = true; // 是否导航 画导航路径时要设为true
        drawParam.mIsOffLine = false; // 是否离线
        drawParam.mRouteMapMode = BizRouteMapMode.BizRouteMapModeMain; // 图模式 主图 或 鹰眼
        drawParam.mRouteScene = RouteLayerScene.RouteLayerSceneNormal; // 路线业务场景 单指线
        drawParam.mIsMultipleMode = true; // 是否是多备选模式
        Logger.i(TAG, "设置主图路线风格 -> " + drawParam);
        mBziRouteControl.setPathDrawStyle(drawParam);
    }

    /**
     * 设置样式风格.
     */
    private void setEyesMapPathDrawStyle() {
        // 路线绘制风格
        RouteDrawStyle drawParam = new RouteDrawStyle();
        drawParam.mIsNavi = false; // 是否导航 画导航路径时要设为true
        drawParam.mIsOffLine = false; // 是否离线
        drawParam.mRouteMapMode = BizRouteMapMode.BizRouteMapModeEagleEye; // 图模式 主图 或 鹰眼
        drawParam.mRouteScene = RouteLayerScene.RouteLayerSceneNormal; // 路线业务场景 单指线
        drawParam.mIsMultipleMode = false; // 是否是多备选模式
        Logger.i(TAG, "设置鹰眼路线风格 -> " + drawParam);
        mBziRouteControl.setPathDrawStyle(drawParam);
    }

    /**
     * 更新路线行程点信息.
     *
     * @param routeLinePoints 路线图层参数
     */
    private void setPathPoints(RouteLinePoints routeLinePoints) {
        Logger.i(TAG, "设置路线行程点的信息 -> " + routeLinePoints);
        RoutePoints pathPoints = GsonUtils.convertToT(routeLinePoints, RoutePoints.class);
        mBziRouteControl.setPathPoints(pathPoints);
    }

    /**
     * 更新引导路线数据.
     *
     * @param pathInfoList 路线信息
     * @param selectIndex  默认选中路线
     */
    private void setPathInfos(ArrayList<?> pathInfoList, int selectIndex) {
        long pathCount = pathInfoList == null ? 0 : pathInfoList.size();
        ArrayList<BizPathInfoAttrs> bizPathInfoAttrs = new ArrayList<>();
        for (int i = 0; i < pathCount; i++) {
            PathInfo pathInfo = (PathInfo) pathInfoList.get(i);
            if (null == pathInfo || !pathInfo.isValid()) return;
            BizPathInfoAttrs bizPath = new BizPathInfoAttrs();
            bizPath.mPathInfo = pathInfo;
            bizPath.mDrawAtts = new BizRouteDrawCtrlAttrs();
            bizPath.mDrawAtts.mIsDrawPath = true;//是否要绘制
            bizPath.mDrawAtts.mIsDrawPathCamera = false;//是否绘制电子眼
            bizPath.mDrawAtts.mIsDrawPathTrafficLight = true; //是否要绘制路线上的交通灯
            bizPath.mDrawAtts.mIsDrawArrow = true;//是否要绘制转向箭头
            bizPath.mDrawAtts.mIsVisible = true;//是否要显示
            bizPath.mDrawAtts.mIsTrafficEventOpen = true;//是否要打开交通事件显示开关，默认为开
            bizPath.mDrawAtts.mIsHighLightRoadName = true;
            bizPathInfoAttrs.add(bizPath);
        }
        Logger.i(TAG, "设置路线 更新引导路线数据 -> " + bizPathInfoAttrs, "默认选中路线 -> " + selectIndex);
        mBziRouteControl.setPathInfos(bizPathInfoAttrs, selectIndex);
    }

    /**
     * 绘制路线以及路线上的元素.
     */
    private void updatePaths() {
        mBziRouteControl.updatePaths();
        mBziRouteControl.updatePathArrow();
    }

    /**
     * 切换选中的路线，同时改变当前选中路线的样式
     *
     * @param index 第几条路线
     * @return 切换是否成功，超过当前路线个数返回false @thread multi
     */
    public boolean switchSelectedPath(int index) {
        return mBziRouteControl.switchSelectedPath(index);
    }

    /**
     * @param segmentsIndexs 设置转向箭头要显示导航段
     */
    public void setPathArrowSegment(ArrayList<Long> segmentsIndexs) {
        mBziRouteControl.setPathArrowSegment(segmentsIndexs);
    }

    /**
     * 更新路线上的箭头
     */
    public void updatePathArrow() {
        mBziRouteControl.updatePathArrow();
    }

    /**
     * 展示路线上的服务区数据
     */
    public void showRestArea(ArrayList<?> pathInfoList, int index) {
        ArrayList<BizRouteRestAreaInfo> restAreaInfos = new ArrayList<>();
        if (ConvertUtils.isEmpty(pathInfoList)) {
            mBziRouteControl.updateRouteRestAreaInfo(restAreaInfos);
            return;
        }

        PathInfo pathInfo = (PathInfo) pathInfoList.get(index);
        ArrayList<RestAreaInfo> restAreas = pathInfo.getRestAreas(0, 20);
        for (RestAreaInfo info : restAreas) {
            BizRouteRestAreaInfo bizRouteRestAreaInfo = new BizRouteRestAreaInfo();
            bizRouteRestAreaInfo.restAreaLabelInfo = info;
            restAreaInfos.add(bizRouteRestAreaInfo);
        }
        mBziRouteControl.updateRouteRestAreaInfo(restAreaInfos);
    }

    /**
     * 展示路线上的天气数据
     */
    public void showWeatherView(ArrayList<?> weatherLabelItem) {
        ArrayList<BizRouteWeatherInfo> weatherInfos = new ArrayList<>();
        if (ConvertUtils.isEmpty(weatherLabelItem)) {
            mBziRouteControl.updateRouteWeatherInfo(weatherInfos);
            return;
        }
        for (int i = 0; i < weatherLabelItem.size(); i++) {
            BizRouteWeatherInfo info = new BizRouteWeatherInfo();
            info.weatherLabelInfo = (WeatherLabelItem) weatherLabelItem.get(i);
            weatherInfos.add(info);
        }
        mBziRouteControl.updateRouteWeatherInfo(weatherInfos);

    }

    @Override
    protected void unInit() {
        removeLayerClickListener();
        if (!ConvertUtils.isEmpty(mBziRouteControl)) mBziRouteControl = null;
    }

    /**
     * 是否打开自动比例尺
     *
     * @param isOpen 开关状态
     */
    public void openDynamicLevel(boolean isOpen) {
        mBziRouteControl.openDynamicLevel(isOpen);
        //设置地图中心点，不根据自动比例尺移动
        mBziRouteControl.openDynamicCenter(false);
    }

    // TODO 参照现代的代码实现，如果有问题请适当修改
    private DynamicLevelParam assembleDynamicLevelParam() {
        DynamicLevelParam param = new DynamicLevelParam();
        param.mLongDisToNaviPoint = 600;
        param.mSpeedWayMinLevel2DCarUp = 14.0f;
        param.mSpeedWayMinLevel3DCarUp = 14.0f;
        param.mPitchFar3DCarUp = 50.0f;
        param.mNaviPointOffsetToScreenTop = GetDynamicLevelTopOffsetValue(false);
        return param;
    }

    /**
     * @return 导航点偏移位置
     * @brief 计算导航点偏移位置
     * @param[in] isShowCrossImage     是否显示路口图
     */
    private int GetDynamicLevelTopOffsetValue(boolean isShowCrossImage) {
        long screenWidth = mMapView.getMapviewPort().screenWidth;
        long screenHeight = mMapView.getMapviewPort().screenHeight;
        float percent = 0.26f;
        if (screenWidth < screenHeight) {
            if (screenWidth < 930) {
                percent = isShowCrossImage ? 0.5f : 0.26f;
            } else {
                if (screenHeight > 930) {
                    percent = 0.05f;
                }
            }
        } else {
            if (screenHeight > 930) {
                percent = 0.05f;
            } else if (screenHeight >= 640) {
                percent = 0.1f;
            } else {
                percent = 0.26f;
            }
        }
        return (int) (screenHeight * percent);
    }

    public int setDynamicLevelLock(boolean isLock, int type) {
        return mBziRouteControl.setDynamicLevelLock(isLock, type);
    }

    public void resetDynamicLevel(int type) {
        mBziRouteControl.resetDynamicLevel(type);
    }

    public int openDynamicCenter(boolean changeCenter) {
        return mBziRouteControl.openDynamicCenter(changeCenter);
    }

    public boolean getDynamicLevelLock(int type) {
        return mBziRouteControl.getDynamicLevelLock(type);
    }

    public float getDynamicLevelMapHeadDegree(int type) {
        return mBziRouteControl.getDynamicLevelMapHeadDegree(type);
    }
}
