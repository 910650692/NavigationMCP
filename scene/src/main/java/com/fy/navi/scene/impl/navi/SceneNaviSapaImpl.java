package com.fy.navi.scene.impl.navi;

import static com.fy.navi.service.adapter.navi.NaviConstant.SapaItemsType.SPAS_LIST;

import android.annotation.SuppressLint;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.ui.navi.SceneNaviSapaView;
import com.fy.navi.scene.ui.navi.component.ComponentHighwayService;
import com.fy.navi.scene.ui.navi.component.ComponentTollStation;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.BaseApplication;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SceneNaviSapaImpl extends BaseSceneModel<SceneNaviSapaView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    //布局可见性 0:只有服务区 1:只有收费站 2:第一个是服务区 3：第一个是收费站
    public ObservableField<Integer> viewVisible;
    // 只有服务区的名称
    public ObservableField<String> onlyServiceName;
    // 只有服务区的时候加油站的可见性
    public ObservableField<Boolean> onlyServiceGasStationVisible;
    // 只有服务区的时候餐饮的可见性
    public ObservableField<Boolean> onlyServiceCanteenVisible;
    // 只有服务区的时候卫生间的可见性
    public ObservableField<Boolean> onlyServiceLavatoryVisible;
    // 只有服务区的时候汽修的可见性
    public ObservableField<Boolean> onlyServiceMaintenanceVisible;
    // 只有服务区的时候购物的可见性
    public ObservableField<Boolean> onlyServiceBuyVisible;
    // 只有服务区的时候住宿的可见性
    public ObservableField<Boolean> onlyServiceHotelVisible;
    // 只有服务区的时候充电站的可见性
    public ObservableField<Boolean> onlyServiceChargeStationVisible;
    // 只有服务区的时候角标的显示名称
    public ObservableField<String> onlyServiceTag;
    // 只有服务区的时候剩余距离
    public ObservableField<String> onlyServiceDistance;
    // 只有收费站的名称
    public ObservableField<String> onlyTollName;
    // 只有收费站的etc可见性
    public ObservableField<Boolean> onlyTollEtcVisible;
    // 只有收费站的支付宝可见性
    public ObservableField<Boolean> onlyTollAlipayVisible;
    //只有收费站的时候角标的显示名称
    public ObservableField<String> onlyTollTag;
    // 只有收费站的时候剩余距离
    public ObservableField<String> onlyTollDistance;
    // 第一个是服务区时服务区的名称
    public ObservableField<String> firstServiceName;
    // 第一个是服务区的时候加油站的可见性
    public ObservableField<Boolean> firstServiceGasStationVisible;
    // 第一个是服务区的时候餐饮的可见性
    public ObservableField<Boolean> firstServiceCanteenVisible;
    // 第一个是服务区的时候卫生间的可见性
    public ObservableField<Boolean> firstServiceLavatoryVisible;
    // 第一个是服务区的时候汽修的可见性
    public ObservableField<Boolean> firstServiceMaintenanceVisible;
    // 第一个是服务区的时候购物的可见性
    public ObservableField<Boolean> firstServiceBuyVisible;
    // 第一个是服务区的时候住宿的可见性
    public ObservableField<Boolean> firstServiceHotelVisible;
    // 第一个是服务区的时候充电站的可见性
    public ObservableField<Boolean> firstServiceChargeStationVisible;
    // 第一个是服务区的时候角标的显示名称
    public ObservableField<String> firstServiceTag;
    // 第一个是服务区的时候剩余距离
    public ObservableField<String> firstServiceDistance;
    // 第一个是服务区的时候收费站的名称
    public ObservableField<String> firstServiceTollName;
    // 第一个是服务区的时候收费站的etc可见性
    public ObservableField<Boolean> firstServiceTollEtcVisible;
    // 第一个是服务区的时候收费站的支付宝可见性
    public ObservableField<Boolean> firstServiceTollAlipayVisible;
    //第一个是服务区的时候收费站角标显示的名称
    public ObservableField<String> firstServiceTollTag;
    //第一个是服务区时收费站的剩余距离
    public ObservableField<String> firstServiceTollDistance;
    // 第一个是收费站时服务区的名称
    public ObservableField<String> firstTollServiceName;
    // 第一个是收费站时的时候加油站的可见性
    public ObservableField<Boolean> firstTollServiceGasStationVisible;
    // 第一个是收费站的时候餐饮的可见性
    public ObservableField<Boolean> firstTollServiceCanteenVisible;
    // 第一个是收费站的时候卫生间的可见性
    public ObservableField<Boolean> firstTollServiceLavatoryVisible;
    // 第一个是收费站的时候汽修的可见性
    public ObservableField<Boolean> firstTollServiceMaintenanceVisible;
    // 第一个是收费站的时候购物的可见性
    public ObservableField<Boolean> firstTollServiceBuyVisible;
    // 第一个是收费站的时候住宿的可见性
    public ObservableField<Boolean> firstTollServiceHotelVisible;
    // 第一个是收费站的时候充电站的可见性
    public ObservableField<Boolean> firstTollServiceChargeStationVisible;
    // 第一个是收费站的时候服务区角标的显示名称
    public ObservableField<String> firstTollServiceTag;
    // 第一个是收费站时服务区剩余距离
    public ObservableField<String> firstTollServiceDistance;
    // 第一个是收费站的名称
    public ObservableField<String> firstTollName;
    // 第一个收费站的etc可见性
    public ObservableField<Boolean> firstTollEtcVisible;
    // 第一个是收费站的支付宝可见性
    public ObservableField<Boolean> firstTollAlipayVisible;
    //第一个是收费站角标显示的名称
    public ObservableField<String> firstTollTag;
    // 第一个是收费站时的剩余距离
    public ObservableField<String> firstTollDistance;
    private int mFirstSflWidth = 480;
    public static final String VIA = "经";
    public static final String UN_BUILDING = "非建设中";
    public static final String BUILDING = "建设中";
    public static final String UN_INVESTIGATION = "未调查";
    public static final String REMODELING = "装修中";
    public static final String TEMPORARILY_CLOSED = "暂停营业";
    private ComponentTollStation mTollStationFirst, mTollStationSecond;
    private ComponentHighwayService mHighwayServiceFirst, mHighwayServiceSecond;
    public static final String TYPE_ONE_HIGHWAY = "one_highway";
    public static final String TYPE_ONE_TOLL = "one_toll";
    public static final String TYPE_ALL_HIGHWAY = "all_highway";
    public static final String TYPE_ALL_TOLL = "all_toll";
    public static final String TYPE_FIRST_HIGHWAY = "first_highway";
    public static final String TYPE_FIRST_TOLL = "first_toll";
    public static final String TYPE_NONE = "";
    private String mCurrentType = TYPE_NONE;

    public SceneNaviSapaImpl(SceneNaviSapaView mScreenView) {
        super(mScreenView);
        viewVisible = new ObservableField<>(0);
        onlyServiceName = new ObservableField<>("");
        onlyServiceGasStationVisible = new ObservableField<>(false);
        onlyServiceCanteenVisible = new ObservableField<>(false);
        onlyServiceLavatoryVisible = new ObservableField<>(false);
        onlyServiceMaintenanceVisible = new ObservableField<>(false);
        onlyServiceBuyVisible = new ObservableField<>(false);
        onlyServiceHotelVisible = new ObservableField<>(false);
        onlyServiceChargeStationVisible = new ObservableField<>(false);
        onlyServiceTag = new ObservableField<>("");
        onlyTollName = new ObservableField<>("");
        onlyTollEtcVisible = new ObservableField<>(false);
        onlyTollAlipayVisible = new ObservableField<>(false);
        onlyTollTag = new ObservableField<>("");
    }

    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        Logger.i(TAG, "SceneNaviSAPAImpl onNaviSAPAInfo type: " + sapaInfoEntity.getType() +
                ",mCurrentType：" + mCurrentType);
        switch (sapaInfoEntity.getType()) {
            case SPAS_LIST://服务区
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    // 显示只有服务区的布局
                    viewVisible.set(0);
                    updateOnlyServiceData(sapaInfoEntity.getList().get(0));
                    updateSceneVisible(true);
                }
                break;
            case NaviConstant.SapaItemsType.TOLL_STATION_LIST://收费站
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    // 显示只有收费站的布局
                    viewVisible.set(1);
                    updateOnlyTollData(sapaInfoEntity.getList().get(0),
                            sapaInfoEntity.getLaneTypes());
                    updateSceneVisible(true);
                }
                break;
            case NaviConstant.SapaItemsType.TOLL_STATION_AND_SPAS://一个服务区，一个收费站
                if (!ConvertUtils.isEmpty(sapaInfoEntity.getList())) {
                    SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(0);
                    if (sapaItem.getType() == SPAS_LIST) {//第一个是服务区，第二个是收费站
                        updateFirstServiceData(sapaItem, sapaInfoEntity.getList().get(1),
                                sapaInfoEntity.getLaneTypes());
                        updateSceneVisible(true);
                    } else {//第一个是收费站，第二个是服务区
                        updateFirstTollData(sapaItem, sapaInfoEntity.getList().get(1),
                                sapaInfoEntity.getLaneTypes());
                        updateSceneVisible(true);
                    }
                }
                break;
            default:
                resetSapa();
                break;
        }
    }

    @Override
    protected void onDestroy() {
        resetSapa();
        super.onDestroy();
    }

    private void resetSapa() {
//        if (sapaVisible != null) {
//            sapaVisible.set(false);
//        }
        mCurrentType = TYPE_NONE;
        mTollStationFirst = null;
        mTollStationSecond = null;
        mHighwayServiceFirst = null;
        mHighwayServiceSecond = null;
    }

    public void addSceneCallback() {

    }
    private void updateSceneVisible(boolean isVisible){
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_SERVICE_AREA);
    }

    /**
     * @param sapItem 服务区更新需要的数据
     * @param sapItemSecond 收费站更新需要的数据
     */
    public void updateFirstServiceData(SapaInfoEntity.SAPAItem sapItem,
                                       SapaInfoEntity.SAPAItem sapItemSecond,
                                       ArrayList<Integer> laneTypes) {
        //服务区名称
        firstServiceName.set(sapItem.getName());
        updateServiceDetails(sapItem, firstServiceGasStationVisible, firstServiceCanteenVisible,
                firstServiceLavatoryVisible, firstServiceMaintenanceVisible, firstServiceBuyVisible,
                firstServiceHotelVisible);
        tagUpdate(firstServiceTag, sapItem);
        updateDistance(sapItem, firstServiceDistance);
        //收费站名称
        firstServiceTollName.set(sapItemSecond.getName());
        updateTollDetail(laneTypes, firstServiceTollEtcVisible, firstServiceTollAlipayVisible);
        tagUpdate(firstServiceTollTag, sapItemSecond);
        updateDistance(sapItemSecond, firstServiceTollDistance);
    }

    public void updateFirstTollData(SapaInfoEntity.SAPAItem sapItem,
                                    SapaInfoEntity.SAPAItem sapItemSecond,
                                    ArrayList<Integer> laneTypes) {
        //收费站名称
        firstTollName.set(sapItem.getName());
        updateTollDetail(laneTypes, firstTollEtcVisible, firstTollAlipayVisible);
        tagUpdate(firstTollTag, sapItem);
        updateDistance(sapItem, firstTollDistance);
        //服务区名称
        firstTollServiceName.set(sapItemSecond.getName());
        updateServiceDetails(sapItemSecond, firstTollServiceGasStationVisible, firstTollServiceCanteenVisible,
                firstTollServiceLavatoryVisible, firstTollServiceMaintenanceVisible, firstTollServiceBuyVisible,
                firstTollServiceHotelVisible);
        tagUpdate(firstTollServiceTag, sapItemSecond);
        updateDistance(sapItemSecond, firstTollServiceDistance);
    }

    /**
     * @param sapItem 更新只有服务区的数据
     */
    public void updateOnlyServiceData(SapaInfoEntity.SAPAItem sapItem) {
        //服务区名称
        onlyServiceName.set(sapItem.getName());
        updateServiceDetails(sapItem, onlyServiceGasStationVisible, onlyServiceCanteenVisible,
                onlyServiceLavatoryVisible, onlyServiceMaintenanceVisible, onlyServiceBuyVisible,
                onlyServiceHotelVisible);
        tagUpdate(onlyServiceTag, sapItem);
        updateDistance(sapItem, onlyServiceDistance);
    }

    /**
     * 更新只有收费站的数据
     */
    public void updateOnlyTollData(SapaInfoEntity.SAPAItem sapItem, ArrayList<Integer> laneTypes) {
        onlyTollName.set(sapItem.getName());
        updateTollDetail(laneTypes, onlyTollEtcVisible, onlyTollAlipayVisible);
        tagUpdate(onlyTollTag, sapItem);
        updateDistance(sapItem, onlyTollDistance);
    }

    /**
     * 更新可收费类型详情
     */
    private void updateTollDetail(ArrayList<Integer> laneTypes, ObservableField<Boolean> etc,
                                  ObservableField<Boolean> alipay) {
        etc.set(false);
        alipay.set(false);
        for (Integer integer : laneTypes) {
            // TollLaneTypeETC等于2可以显示etc标识
            if (integer == 2) {
                etc.set(true);
            }
            // TollLaneTypeAliPay等于8可以显示支付宝标识
            if (integer == 8) {
                alipay.set(true);
            }
        }
    }

    /**
     * 更新剩余距离信息
     */
    private void updateDistance(SapaInfoEntity.SAPAItem sapItem,
                                ObservableField<String> observable) {
        int distance = sapItem.getRemainDist();
        observable.set(convertMetersToKilometers(distance));
    }

    @SuppressLint("DefaultLocale")
    public String convertMetersToKilometers(int meters) {
        double kilometers = meters / 1000.0;
        if (kilometers < 100) {
            // 保留小数点后一位
            return String.format("%.1f" + mScreenView.getContext().getString(R.string.km),
                    kilometers);
        } else {
            // 保留整数部分
            return String.format("%d" + mScreenView.getContext().getString(R.string.km),
                    (int) kilometers);
        }
    }

    /**
     * 更新服务区详情
     */
    private void updateServiceDetails(SapaInfoEntity.SAPAItem sapItem,
                                      ObservableField<Boolean> gasStation,
                                      ObservableField<Boolean> canteen,
                                      ObservableField<Boolean> lavatory,
                                      ObservableField<Boolean> maintenance,
                                      ObservableField<Boolean> buy,
                                      ObservableField<Boolean> hotel) {
        // 服务区详情
        long sapaDetail = sapItem.getSapaDetail();
        for (int i = 0; i < 6; i++) {
            int nthBit = getNthBit(sapaDetail, i);
            switch (i) {
                case 0:
                    gasStation.set(nthBit == 1);
                    break;
                case 1:
                    canteen.set(nthBit == 1);
                    break;
                case 2:
                    lavatory.set(nthBit == 1);
                    break;
                case 3:
                    maintenance.set(nthBit == 1);
                    break;
                case 4:
                    buy.set(nthBit == 1);
                    break;
                case 5:
                    hotel.set(nthBit == 1);
                    break;
                default:
                    break;
            }
        }
    }

    /**
     * 更新标签
     */
    private void tagUpdate(ObservableField<String> tag, SapaInfoEntity.SAPAItem sapItem) {
        // 角标显示 途经点>维护/关闭类>其他
        // 判断是否是途经点
        List<RouteParam> allPoiParamList = RoutePackage.getInstance().
                getAllPoiParamList(mMapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (Objects.equals(routeParam.poiID, sapItem.getServicePOIID())) {
                tag.set(VIA);
                return;
            }
        }
        //服务区状态:0 非建设中（默认值），1 建设中，2 未调查 3 装修中 4 暂停营业
        int buildingStatus = sapItem.getBuildingStatus();
        switch (buildingStatus) {
            case 0:
                tag.set(UN_BUILDING);
                break;
            case 1:
                tag.set(BUILDING);
                break;
            case 3:
                tag.set(REMODELING);
                break;
            case 4:
                tag.set(TEMPORARILY_CLOSED);
                break;
            default:
                tag.set(UN_INVESTIGATION);
                break;
        }
    }

    public static int getNthBit(long number, int n) {
        // 使用位运算右移n位，然后与1进行与运算，得到第n位的值
        return (int)((number >> n) & 1);
    }
}
