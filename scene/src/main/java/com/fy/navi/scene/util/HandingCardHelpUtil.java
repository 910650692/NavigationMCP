package com.fy.navi.scene.util;

import android.os.CountDownTimer;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;

import java.util.ArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/14
 * Description: [悬挂卡帮助类]
 */
public class HandingCardHelpUtil {
    private static final String TAG = "HandingCardHelpUtil";
    public static final String KEY_WORD_CHARGE_STATION = "充电桩";
    public static final String KEY_WORD_GAS_STATION = "加油站";
    public static final int DEFAULT_PAGE = 1;
    // 8秒无响应自动关闭
    private static final long TOTAL_TIME = 20 * 1000;
    private static final long INTERVAL = 1000;
    // 是否悬挂卡处于可以显示状态
    private boolean chargeCardOnShow = false;
    private boolean gasCardOnShow = false;
    private boolean parkCardOnShow = false;
    // 悬挂卡是否正在计时当中
    private boolean chargeOnTicking = false;
    private boolean gasOnTicking = false;
    private boolean parkOnTicking = false;

    // 倒计时开始时间
    private long chargeCountTime = TOTAL_TIME;
    private long gasCountTime = TOTAL_TIME;
    private long parkCountTime = TOTAL_TIME;
    private int mShowSize = 0;
    private boolean mIsEndThisNavi = false;
    private OnChangeListener mListener;
    public final CountDownTimer mCountDownTimerCharge = new CountDownTimer(chargeCountTime, INTERVAL) {
        @Override
        public void onTick(long millisUntilFinished) {
            chargeOnTicking = true;
            chargeCountTime = millisUntilFinished;
        }

        @Override
        public void onFinish() {
            chargeOnTicking = false;
            changeShowSize(false, HandCardType.CHARGE);
            notifyUiUpdate(HandCardType.CHARGE);
        }
    };

    public final CountDownTimer mCountDownTimerGas = new CountDownTimer(gasCountTime, INTERVAL) {
        @Override
        public void onTick(long millisUntilFinished) {
            gasCountTime = millisUntilFinished;
            gasOnTicking = true;
        }

        @Override
        public void onFinish() {
            gasOnTicking = false;
            changeShowSize(false, HandCardType.GAS);
            notifyUiUpdate(HandCardType.GAS);
        }
    };

    public final CountDownTimer mCountDownTimerPark = new CountDownTimer(parkCountTime, INTERVAL) {
        @Override
        public void onTick(long millisUntilFinished) {
            parkCountTime = millisUntilFinished;
            parkOnTicking = true;
        }

        @Override
        public void onFinish() {
            parkOnTicking = false;
            changeShowSize(false, HandCardType.PARK);
            notifyUiUpdate(HandCardType.PARK);
        }
    };

    /***
     * 开启定时器
     * @param cardType
     * @param needRestTime 是否重置时间
     */
    public void startTimer(final HandCardType cardType, final boolean needRestTime) {
        switch (cardType) {
            case CHARGE -> {
                if (chargeCardOnShow) {
                    chargeCountTime = needRestTime ? TOTAL_TIME : chargeCountTime;
                    mCountDownTimerCharge.cancel();
                    mCountDownTimerCharge.start();
                    Logger.i(TAG, "startTimer:" + cardType.name(), "success", "chargeOnTicking:" + chargeOnTicking);
                }
            }
            case GAS -> {
                if (gasCardOnShow) {
                    gasCountTime = needRestTime ? TOTAL_TIME : gasCountTime;
                    mCountDownTimerGas.cancel();
                    mCountDownTimerGas.start();
                    Logger.i(TAG, "startTimer:" + cardType.name(), "success", "gasOnTicking:" + gasOnTicking);
                }
            }
            case PARK -> {
                if (parkCardOnShow) {
                    parkCountTime = needRestTime ? TOTAL_TIME : parkCountTime;
                    mCountDownTimerPark.cancel();
                    mCountDownTimerPark.start();
                    Logger.i(TAG, "startTimer:" + cardType.name(), "success", "parkOnTicking:" + parkOnTicking);
                }
            }
            default -> {
                throw new UnknownError("未知类型！");
            }
        }
    }

    public void stopTimer(final HandCardType cardType, final boolean needRestTime) {
        switch (cardType) {
            case CHARGE -> {
                mCountDownTimerCharge.cancel();
                chargeCountTime = needRestTime ? TOTAL_TIME : chargeCountTime;
                Logger.i(TAG, "stopTimer-charge");
            }
            case GAS -> {
                mCountDownTimerGas.cancel();
                gasCountTime = needRestTime ? TOTAL_TIME : gasCountTime;
                Logger.i(TAG, "stopTimer-gas");
            }
            case PARK -> {
                mCountDownTimerPark.cancel();
                parkCountTime = needRestTime ? TOTAL_TIME : parkCountTime;
                Logger.i(TAG, "stopTimer-park");
            }
            default -> {
                throw new UnknownError("未知类型！");
            }
        }
    }

    /***
     *全部重置
     */
    public synchronized void resetAll() {
        mCountDownTimerCharge.cancel();
        mCountDownTimerGas.cancel();
        mCountDownTimerPark.cancel();
        chargeCountTime = TOTAL_TIME;
        gasCountTime = TOTAL_TIME;
        parkCountTime = TOTAL_TIME;
        mShowSize = 0;
        chargeCardOnShow = false;
        gasCardOnShow = false;
        parkCardOnShow = false;
        mIsEndThisNavi = true;
    }

    public HandingCardHelpUtil() {
    }

    /***
     *
     * @param searchResultEntity
     * @return
     */
    public ArrayList<PoiInfoEntity> getStationList(final SearchResultEntity searchResultEntity) {
        if (ConvertUtils.isNull(searchResultEntity) || ConvertUtils.isEmpty(searchResultEntity.getPoiList())) {
            return new ArrayList<>();
        } else {
            return (ArrayList<PoiInfoEntity>) searchResultEntity.getPoiList();
        }
    }

    /***
     *
     * @param searchResultEntity
     * @param endPoiInfo
     * @return
     */
    public ArrayList<PoiInfoEntity> getParkList(final SearchResultEntity searchResultEntity, final PoiInfoEntity endPoiInfo) {
        final ArrayList<PoiInfoEntity> dataList = new ArrayList<>();
        final ArrayList<PoiInfoEntity> tmpList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(searchResultEntity.getPoiList()) || !ConvertUtils.isNull(endPoiInfo)) {
            tmpList.addAll(searchResultEntity.getPoiList());
            // 按照距离排序
            tmpList.sort((o1, o2) -> getRealDis(o1) >= getRealDis(o2) ? 0 : 1);

            // -终点是停车场但停车位紧张导致的推荐，【当前终点】放在列表第二位。 UE:1.12-2
            if (endIsParking(endPoiInfo)) {
                if (ConvertUtils.isEmpty(tmpList)) {
                    dataList.add(endPoiInfo);
                } else if (tmpList.size() == 1) {
                    dataList.addAll(tmpList);
                    dataList.add(endPoiInfo);
                } else {
                    for (int i = 0; i < tmpList.size(); i++) {
                        if (i == 1) {
                            dataList.add(endPoiInfo);
                        }
                        dataList.add(tmpList.get(i));
                    }
                }
            } else {
                dataList.addAll(tmpList);
            }
        }
        ArrayList<PoiInfoEntity> finalData = new ArrayList<>();
        finalData.addAll(dataList.size() > 3 ? dataList.subList(0, 3) : dataList);
        return finalData;
    }

    /***
     * 判断距离是否满足预期
     * @param distance
     * @param distanceCondition
     * @return
     */
    public boolean isEligible(int distance, int distanceCondition) {
        return distance <= distanceCondition;
    }

    /***
     * 判断终点是否改变
     * @return true 代表已改变， false未改变
     */
    public boolean endIsChanged(final RouteParam last, final RouteParam current) {
        if (ConvertUtils.isNull(last) || ConvertUtils.isNull(current)) {
            return true;
        }
        return last.getRealPos().getLat() != current.getRealPos().getLat()
                || last.getRealPos().getLon() != current.getRealPos().getLon();
    }

    private double getRealDis(PoiInfoEntity poiInfo) {
        final String dis = poiInfo.getDistance();
        double tmp;
        try {
            if (ConvertUtils.isEmpty(dis)) {
                tmp = 0;
            } else if (dis.contains("千米") || dis.contains("公里")) {
                dis.replaceAll("千米", "");
                dis.replaceAll("公里", "");
                tmp = Double.parseDouble(dis) * 1000;
            } else if (dis.contains("米")) {
                dis.replaceAll("米", "");
                tmp = Double.parseDouble(dis);
            } else {
                tmp = 0;
            }
        } catch (Exception e) {
            tmp = 0;
        }
        return tmp;
    }

    public void onDestroy() {
        mCountDownTimerCharge.cancel();
        mCountDownTimerGas.cancel();
        mCountDownTimerPark.cancel();
    }

    public synchronized int getHandCardNeedShowSize() {
        Logger.d(TAG, "getHandCardNeedShowSize:" + mShowSize);
        return mShowSize;
    }

    public synchronized void changeShowSize(final boolean isAdd, HandCardType type) {
        Logger.i(TAG, "changeShowSize:" + mShowSize, "isAdd:" + isAdd, "type:" + type.name());
        switch (type) {
            case CHARGE -> chargeCardOnShow = isAdd;
            case GAS -> gasCardOnShow = isAdd;
            case PARK -> parkCardOnShow = isAdd;
            default -> {
            }
        }
        if (isAdd) {
            mShowSize++;
        } else {
            mShowSize--;
        }
        if (mShowSize < 0) {
            mShowSize = 0;
        }
    }

    public void registerListener(final OnChangeListener listener) {
        this.mListener = listener;
    }

    public void unRegisterListener() {
        this.mListener = null;
    }

    private void notifyUiUpdate(HandCardType type) {
        if (mListener != null) {
            mListener.onTimerEnd(type);
        }
    }

    public void deleteHandCard(HandCardType type) {
        Logger.i(TAG, "deleteHandCard", "type:" + type.name());
        changeShowSize(false, type);
        notifyUiUpdate(type);
    }

    /***
     * 立即导航成功后回调
     */
    public void rightNowNaviSuccessCallBack() {
        mCountDownTimerCharge.cancel();
        mCountDownTimerGas.cancel();
        mCountDownTimerPark.cancel();
        mShowSize = 0;
    }

    public interface OnChangeListener {
        void onTimerEnd(HandCardType type);
    }

    /***
     * 判断终点是否是停车场
     * @return
     */
    public boolean endIsParking(final PoiInfoEntity poiInfo) {
        final boolean result = !ConvertUtils.isNull(poiInfo)
                && !ConvertUtils.isEmpty(poiInfo.getPoiTag())
                && poiInfo.getPoiTag().contains(AppCache.getInstance().getMApplication().getString(com.fy.navi.scene.R.string.st_quick_search_parking));
        Logger.i(TAG, "endIsParking:" + result);
        return result;
    }

    /***
     * 判断停车场是否紧张
     * -停车位紧张：总车位数<=30个，剩余车位<30% ；总车位数>30个，剩余车位<10% 或 剩余车位少于10个。
     */
    public boolean parkIsCrowed(final PoiInfoEntity poiInfo) {
        if (ConvertUtils.isNull(poiInfo) || ConvertUtils.isEmpty(poiInfo.getParkingInfoList())) {
            return false;
        }
        final ParkingInfo parkingInfo = poiInfo.getParkingInfoList().get(0);
        final int totalSize = parkingInfo.getSpaceTotal();
        final int spaceSize = parkingInfo.getSpaceFree();
        Logger.i(TAG, "parkIsCrowed", "totalSize:" + totalSize, "spaceSize:" + spaceSize);
        if (totalSize <= 0) return false;
        return (totalSize <= 30 && spaceSize * 1f / totalSize < 0.3) || (totalSize > 30 && (spaceSize * 1f / totalSize < 0.1 || spaceSize < 10));
    }
}
