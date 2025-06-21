package com.fy.navi.hmi.cluster.cluster_map;

import static android.view.View.GONE;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.utils.ActivityCloseManager;
import com.fy.navi.utils.OnCloseActivityListener;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BaseClusterViewModel extends BaseViewModel<ClusterActivity, ClusterModel> implements OnCloseActivityListener {
    private static final String TAG = "BaseClusterViewModel";
    public ObservableField<String> arriveTimeField; // 到达时间
    public ObservableField<String> arrivalDayField; // 到达天数
    public ObservableField<String> remainingMileageField; // 剩余距离
    public ObservableField<String> stvNaviRouteNameField; // 当前路名
    public ObservableField<String> mRemainInfoNumber; // 公里数
    public ObservableField<String> mRemainInfoUnit; // 单位
    //道路名是否显示
    public final ObservableField<Boolean> routeNameConstraintLayoutVisibility = new ObservableField<>(false);
    //ETA是否显示
    public final ObservableField<Boolean> remainingMileageConstraintLayoutVisibility = new ObservableField<>(false);

    public BaseClusterViewModel(@NonNull Application application) {
        super(application);
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "BaseClusterViewModel initialized");
        ActivityCloseManager.getInstance().addOnCloseListener(this);
        arriveTimeField = new ObservableField<>();
        arrivalDayField = new ObservableField<>();
        remainingMileageField = new ObservableField<>();
        stvNaviRouteNameField = new ObservableField<>();
        mRemainInfoNumber = new ObservableField<>();
        mRemainInfoUnit = new ObservableField<>();
    }

    @Override
    protected ClusterModel initModel() {
        Logger.d(TAG, "Initializing ClusterModel");
        return new ClusterModel();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy called");
        ActivityCloseManager.getInstance().removeOnCloseListener(this);
        LayerPackage.getInstance().unInitLayer(mView.getMapView().provideMapTypeId());
        MapPackage.getInstance().unBindMapView(mView.getMapView());
        MapPackage.getInstance().destroyMapView(mView.getMapView().provideMapTypeId());
    }

    public void loadMapView(){
        mView.bindMapView();
    }

    public void updateEta(NaviEtaInfo naviEtaInfo) {
        Logger.d(TAG, "Updating ETA info");
        int distance = naviEtaInfo.getRemainDist();
        int time = naviEtaInfo.getRemainTime();

        if (distance <= 0 && time <= 0) {
            Logger.d(TAG, "Distance and time are both zero, skipping update");
            return;
        }
        String mArriveDay = TimeUtils.getArriveDay(time);
        String mArriveTime = TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), time);
        String mRemainInfo = TimeUtils.getRemainingMileage(AppCache.getInstance().getMContext(), distance);
            Logger.i(TAG, "showArriveInfo");
            if (!TextUtils.isEmpty(mArriveTime)) {
                arriveTimeField.set(ConvertUtils.digitToBold(mArriveTime).toString());
            }
            arrivalDayField.set(mArriveDay);
        String mRemainInfoString = ConvertUtils.digitToBold(mRemainInfo).toString();
        if (!TextUtils.isEmpty(mRemainInfoString)) {
            Pattern pattern = Pattern.compile("(\\d+(\\.\\d+)?)(公里|米)");
            Matcher matcher = pattern.matcher(mRemainInfo);
            if (matcher.matches()) {
                try {
                    String numberPart = matcher.group(1); // 匹配数字部分（整数或小数）
                    String unitPart = matcher.group(3);   // 匹配单位“公里”或“米”
                    mRemainInfoNumber.set(numberPart);
                    mRemainInfoUnit.set(unitPart);
                    Logger.d(TAG, "showArriveInfo:numberPart:" + numberPart + ", unitPart:" + unitPart);
                } catch (Exception e) {
                    Logger.e(TAG, "解析剩余里程时发生错误", e);
                }
            } else {
                Logger.w(TAG, "未匹配到有效格式: " + mRemainInfo);
            }
        } else {
            Logger.w(TAG, "mRemainInfo 为空或 null");
        }
    }

    public void updateRouteName(String curRouteName) {
        Logger.d(TAG, "Updating route name");
        stvNaviRouteNameField.set(curRouteName);
    }

    public void updateNaviStatus(boolean isVisible) {
        routeNameConstraintLayoutVisibility.set(false);
        remainingMileageConstraintLayoutVisibility.set(isVisible);
    }
    @Override
    public void onClose(boolean isCluster) {
        if (isCluster){
            //mView.finish();
            if (StackManager.getInstance().getCurrentActivity(MapType.CLUSTER_MAP.name()).isTaskRoot()){
                mView.getRootView().setVisibility(GONE);
                mView.moveTaskToBack(true);
            }
        }
    }
}
