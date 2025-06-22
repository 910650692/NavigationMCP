package com.sgm.navi.hmi.traffic;

import android.app.Application;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.StringRes;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.StringUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.aos.FyCriticism;
import com.sgm.navi.service.define.aos.FyGSubTraEventDetail;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;

import java.util.ArrayList;

/**
 * Author: QiuYaWei
 * Date: 2025/2/27
 * Description: [在这里描述文件功能]
 */
public class BaseTrafficViewModel extends BaseViewModel<TrafficEventFragment, TrafficModel> {
    private static final String TAG = "BaseTrafficViewModel";
    public ObservableField<FyGTraEventDetail> fyGTraEventDetail = new ObservableField<>();
    private PoiInfoEntity poiInfo;
    public ObservableField<Integer> uiState = new ObservableField<>(TrafficEventUiState.IDLE);
    public ObservableField<Boolean> childIndicatorVisible = new ObservableField<>(false);
    public ObservableField<String> timeFromHappenToNow = new ObservableField<>("");
    public ObservableField<String> thumbNumber = new ObservableField<>("");
    public ObservableField<String> unThumbNumber = new ObservableField<>("");
    public ObservableField<Boolean> thumbPrimaseEnable = new ObservableField<>(false);
    public ObservableField<String> straightDis = new ObservableField<>("");
    public ObservableField<String> currentIndicator = new ObservableField<>("");
    public ObservableField<Boolean> preEnable = new ObservableField<>(true);
    public ObservableField<Boolean> nextEnable = new ObservableField<>(true);
    public ObservableField<String> mLoadingDesc = new ObservableField<>("");

    private int currentIndex = 0;
    private boolean isDoThumb = false; // 去点赞

    public BaseTrafficViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected TrafficModel initModel() {
        return new TrafficModel();
    }

    public void queryTrafficEvent(PoiInfoEntity entity) {
        currentIndex = 0;
        if (fyGTraEventDetail.get() == null || !fyGTraEventDetail.get().isRequestSuccess || poiInfo != entity) {
            uiState.set(TrafficEventUiState.LOADING);
            mLoadingDesc.set(AppCache.getInstance().getMContext().getString(R.string.limit_loading));
            mModel.queryTrafficEventInfo(entity);
            this.poiInfo = entity;
        } else {
            updateUi(fyGTraEventDetail.get(), false);
        }
    }

    public Action rootClick = () -> {
    };// 防止点击穿透

    public Action closeSelf = () -> {
        closeFragment(true);
    };

    // 首次进入，数据加载失败重试
    public Action retryLoadAll = () -> {
        queryTrafficEvent(poiInfo);
    };

    // 上报交通事件，确认存在
    public Action uploadTrafficEventThumb = () -> {
        isDoThumb = true;
        mModel.uploadTrafficEvent(true, getCurrentEvent());
        if(NaviStatusPackage.getInstance().isGuidanceActive()) sendBuryPointForReport(true);
    };

    // 上报交通事件，确认不存在
    public Action uploadTrafficEventUnThumb = () -> {
        isDoThumb = false;
        mModel.uploadTrafficEvent(false, getCurrentEvent());
        if(NaviStatusPackage.getInstance().isGuidanceActive()) sendBuryPointForReport(false);
    };

    // 查看上一个事件
    public Action toPre = () -> {
        if (!fyGTraEventDetail.get().canScroller() || currentIndex <= 0) return;
        currentIndex--;
        updateUi(fyGTraEventDetail.get(), true);
    };

    // 查看下一个事件
    public Action toNext = () -> {
        if (!fyGTraEventDetail.get().canScroller() || fyGTraEventDetail.get().subcount - 1 <= currentIndex)
            return;
        currentIndex++;
        updateUi(fyGTraEventDetail.get(), true);
    };

    // 查看大图
    public Action openImageDetail = () -> {
        ArrayList<String> list = new ArrayList<>();
        if (ConvertUtils.isEmpty(fyGTraEventDetail.get().subinfo)) {
            if (!TextUtils.isEmpty(fyGTraEventDetail.get().picurl)) {
                list.add(fyGTraEventDetail.get().picurl);
            }
        } else {
            if (!TextUtils.isEmpty(fyGTraEventDetail.get().subinfo.get(currentIndex).picurl)) {
                list.add(fyGTraEventDetail.get().subinfo.get(currentIndex).picurl);
            }
        }
        if (list.isEmpty()) return;
        BigPicDetailDialog detailDialog = new BigPicDetailDialog(mView.getActivity(), list);
        detailDialog.show();
    };

    // 赞/踩 接口回调
    public void onTrafficUploadFinished(boolean isSuccess) {
        Logger.e(TAG, "onTrafficUploadFinished：" + isSuccess);
        int praise = getCurrentEvent().praise;
        int criticism = getCurrentEvent().criticism;

        if (isSuccess) {
            thumbPrimaseEnable.set(false);
            if (isDoThumb) {
                thumbNumber.set(getFormatThumb(R.string.traffic_increase_format, praise + 1));
            } else {
                unThumbNumber.set(getFormatThumb(R.string.traffic_decrease_format, criticism + 1));
            }
            updatePriseOrCriticism();
        } else {
            Logger.e(TAG, "onTrafficUploadFinished failed!");
        }
    }

    private void updatePriseOrCriticism() {
        if (isDoThumb) {
            if (fyGTraEventDetail.get().hasChild()) {
                fyGTraEventDetail.get().subinfo.get(currentIndex).praise = fyGTraEventDetail.get().subinfo.get(currentIndex).praise + 1;
            } else {
                fyGTraEventDetail.get().praise = fyGTraEventDetail.get().praise + 1;
            }
        } else {
            if (fyGTraEventDetail.get().hasChild()) {
                fyGTraEventDetail.get().subinfo.get(currentIndex).criticism = fyGTraEventDetail.get().subinfo.get(currentIndex).criticism + 1;
            } else {
                fyGTraEventDetail.get().criticism = fyGTraEventDetail.get().criticism + 1;
            }
        }
    }

    public void updateUi(final FyGTraEventDetail gTraEventDetail, boolean isFromChild) {
        Logger.i(TAG, "updateUi:" + (gTraEventDetail != null), "isSuccess:" + gTraEventDetail.isRequestSuccess);
        mLoadingDesc.set(gTraEventDetail.isRequestSuccess ? AppCache.getInstance().getMContext().getString(R.string.limit_loading)
                : AppCache.getInstance().getMContext().getString(R.string.limit_load_fail));
        if (!isFromChild) {
            uiState.set(gTraEventDetail.isRequestSuccess ? TrafficEventUiState.SUCCESS : TrafficEventUiState.ERROR);
            if (!gTraEventDetail.isRequestSuccess) {
                return;
            }
            this.fyGTraEventDetail.set(gTraEventDetail);
            childIndicatorVisible.set(
                    gTraEventDetail != null &&
                            gTraEventDetail.isRequestSuccess &&
                            gTraEventDetail.canScroller()
            );
        }
        preEnable.set(currentIndex > 0 && fyGTraEventDetail.get().canScroller());
        nextEnable.set(currentIndex < fyGTraEventDetail.get().subcount - 1 && fyGTraEventDetail.get().canScroller());

        // 获取子类对象
        FyGSubTraEventDetail subTraEventDetail;
        if (fyGTraEventDetail.get().hasChild()) {
            subTraEventDetail = fyGTraEventDetail.get().getSubinfo().get(currentIndex);
        } else {
            subTraEventDetail = GsonUtils.convertToT(fyGTraEventDetail.get(), FyGSubTraEventDetail.class);
        }
        currentIndicator.set((currentIndex + 1) + "/" + fyGTraEventDetail.get().subcount);
        timeFromHappenToNow.set(TimeUtils.switchTime(StringUtils.stringToLong(subTraEventDetail.getLastupdate())));
        thumbNumber.set(getFormatThumb(R.string.traffic_increase_format, subTraEventDetail.praise));
        unThumbNumber.set(getFormatThumb(R.string.traffic_decrease_format, subTraEventDetail.criticism));

        mModel.queryDynamicPraise(subTraEventDetail.id);
        // 计算直线距离
        double distance = mModel.calcStraightDistance(new GeoPoint(poiInfo.getPoint().getLon(), poiInfo.getPoint().getLat())) / 1000f;
        straightDis.set(String.format(AppCache.getInstance().getMContext().getString(R.string.traffic_distance_format), distance));

        ThreadManager.getInstance().postUi(() -> {
            mView.updateUi(subTraEventDetail);
        });
    }


    private String getFormatThumb(@StringRes int formatStr, int number) {
        if (number > 99) return String.format(AppCache.getInstance().getMContext().getString(formatStr), "99+");
        else return String.format(AppCache.getInstance().getMContext().getString(formatStr), number);
    }

    /***
     * -1：未赞踩过
     * 0：踩
     * 1：赞
     * @param fyCriticism
     */
    public void updatePrimaseStatus(FyCriticism fyCriticism) {
        Logger.i(TAG, "updatePrimaseStatus", "isSuccess:" + fyCriticism.isRequestSuccess);
        if (fyCriticism.isRequestSuccess) {
            thumbPrimaseEnable.set(fyCriticism.status == -1);
        } else {
            thumbPrimaseEnable.set(false);
        }
    }

    // 获取当前事件
    private FyGSubTraEventDetail getCurrentEvent() {
        FyGSubTraEventDetail fyGSubTraEventDetail = null;
        if (fyGTraEventDetail.get() == null) return null;
        if (fyGTraEventDetail.get().hasChild()) {
            fyGSubTraEventDetail = fyGTraEventDetail.get().subinfo.get(currentIndex);
        } else {
            fyGSubTraEventDetail = GsonUtils.convertToT(fyGTraEventDetail.get(), FyGSubTraEventDetail.class);
        }
        return fyGSubTraEventDetail;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_REPORT_SELECT)
    private void sendBuryPointForReport(boolean isExist){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_ROUTE_PREFERENCE, isExist ? BuryConstant.CommonText.EXIST : BuryConstant.CommonText.NOT_EXIST)
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, mView.getTrafficType())
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }
}
