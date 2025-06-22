package com.fy.navi.service.utils;


import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.patac.hmi.privacy.domain.repository.PrivacyDataSourceRepository;
import com.patac.hmi.privacy.model.entity.PrivacyBean;
import com.patac.hmi.privacy.model.proxy.IPrivacyPermission;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * @author: WangLei
 * $Revision.1.0\$
 * Date: 2025/6/16
 * Description: [对接Settings隐私协议]
 */
public class SettingsPrivacyManager implements IPrivacyPermission {

    private final String TAG = SettingsPrivacyManager.class.getSimpleName();
    private static final String DATE_FORMAT = "yyyy年M月d日";
    //当前应用包名
    private String mPackageName;
    //是否同意一年
    private boolean mIsOneYear = false;
    //定位权限到期时间
    private String mEndDate = "";


    public static SettingsPrivacyManager getInstance() {
        return InstanceHolder.INSTANCE;
    }

    private static final class InstanceHolder {
        static final SettingsPrivacyManager INSTANCE = new SettingsPrivacyManager();
    }

    private SettingsPrivacyManager() {

    }

    /**
     * 敏感信息权限回调.
     *
     * @param privacyBeans 权限状态
     */
    @Override
    public void permissionCallBack(List<PrivacyBean> privacyBeans) {
        processMapLocationState(privacyBeans);
    }

    /**
     * 隐私协议管理初始化.
     */
    public void init() {
        mPackageName = AppCache.getInstance().getMContext().getPackageName();
        Logger.d(TAG, "PrivacyManager init", mPackageName);
        //注册LOCATION权限变化通知
        try {
            PrivacyDataSourceRepository.getInstance().registerPrivacyPermissionChangeListener(
                    PrivacyBean.Privacy.LOCATION, SettingsPrivacyManager.this);
            //获取Map所有申请的权限状态
            List<PrivacyBean> locationApps = PrivacyDataSourceRepository.getInstance()
                    .getAppAllPrivacyList(mPackageName);
            processMapLocationState(locationApps);
        } catch (Exception e) {
            Logger.e(TAG, e.getMessage());
        }
    }

    /**
     * 根据当前定位权限App现状集合获取Map定位权限现状.
     *
     * @param privacyBeanList 所有申请某个权限的应用集合.
     */
    private void processMapLocationState(List<PrivacyBean> privacyBeanList) {
        if (null == privacyBeanList || privacyBeanList.isEmpty()) {
            return;
        }

        Logger.d(TAG, "locationAppSize", privacyBeanList.size());
        for (PrivacyBean privacyBean : privacyBeanList) {
            if (null != privacyBean && null != privacyBean.getPkgName()
                    && privacyBean.getPkgName().equals(mPackageName)
                    && PrivacyBean.Privacy.LOCATION == privacyBean.getPrivacy()) {
                formatLocationMsg(privacyBean.getExpiredTime());
            }
        }
    }

    /**
     * 根据权限到期时间格式化权限信息.
     *
     * @param expiredTime 到期时间.
     */
    private void formatLocationMsg(long expiredTime) {
        Logger.d(TAG, "receive expiredTime", expiredTime);
        if (expiredTime > 0L) {
            mIsOneYear = true;
            LocalDate current = LocalDate.now();
            LocalDate nextYear = current.plusYears(1);
            long oneYearMillis = nextYear.atStartOfDay(ZoneId.systemDefault()).toInstant().toEpochMilli();
            final DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DATE_FORMAT);
            mEndDate = Instant
                    .ofEpochMilli(Math.min(oneYearMillis , expiredTime * 1000))
                    .atZone(ZoneId.systemDefault())
                    .format(formatter);
        } else {
            mIsOneYear = false;
            mEndDate = "";
        }
        Logger.d(TAG, "endDate", mEndDate);
        SettingPackage.getInstance().dispatchLocationPrivacyStatus(mIsOneYear);
    }

    /**
     * 获取设置隐私协议状态.
     *
     * @return  true-同意一年  false-用不
     */
    public boolean getLocationPrivacyStatus() {
        return mIsOneYear;
    }

    /**
     * 获取隐私协议到期时间.
     *
     * @return String，格式化之后的授权到期时间，eg:xxxx年xx月xx日
     */
    public String getEndDate() {
        return mEndDate;
    }

    /**
     * 设置隐私协议授权状态.
     *
     * @param oneYear true-同意一年  false-用不.
     *
     */
    public void setLocationPrivacyStatus(final boolean oneYear) {
        List<PrivacyBean.Privacy> privacyList = new ArrayList<>();
        privacyList.add(PrivacyBean.Privacy.LOCATION);
        try {
            final boolean result = PrivacyDataSourceRepository
                    .getInstance().setPrivacyAuthorityUnCertified(mPackageName, privacyList, oneYear);
            Logger.d(TAG, "setLocationPrivacy", oneYear, result);
        } catch (Exception e) {
            Logger.e(TAG, "setLocationPrivacy", e.getMessage());
        }
    }

}
