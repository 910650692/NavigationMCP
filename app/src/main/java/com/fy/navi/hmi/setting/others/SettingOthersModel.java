package com.fy.navi.hmi.setting.others;

import com.android.utils.file.FileUtils;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.define.user.wechat.BLResponseBean;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.wechat.WeChatCallBack;
import com.fy.navi.service.logicpaket.user.wechat.WeChatPackage;
import com.fy.navi.ui.base.BaseModel;

import java.io.File;
import java.util.List;

public class SettingOthersModel extends BaseModel<SettingOthersViewModel> 
        implements SettingUpdateObservable.SettingUpdateObserver, AccountCallBack, WeChatCallBack {

    private final AccountPackage mAccountPackage;
    private final SettingPackage mSettingPackage;
    private final WeChatPackage mWeChatPackage;
    private final SettingManager mSettingManager;
    private final HistoryManager mHistoryManager;
    private static final String MODEL_NAME = "SettingOthersModel";

    private final String[] mDirPaths = {
            GBLCacheFilePath.BLS_LOG
    };

    public SettingOthersModel() {
        mAccountPackage = AccountPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mWeChatPackage = WeChatPackage.getInstance();
        mSettingManager = SettingManager.getInstance();
        mSettingManager.init();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
    }
    @Override
    public void onCreate() {
        super.onCreate();
        SettingUpdateObservable.getInstance().addObserver(MODEL_NAME, this);
        mAccountPackage.registerCallBack(MODEL_NAME,this);
        mWeChatPackage.registerCallBack(MODEL_NAME, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mWeChatPackage.unRegisterCallBack(MODEL_NAME);
    }

    /**
     * 初始化视图
     */
    public void initView() {
        mViewModel.updatePrivacyStatus(mSettingPackage.getPrivacyStatus());
        mViewModel.updateUserInfo(mAccountPackage.getUserInfo().nickname, mAccountPackage.getUserInfo().avatar);
        final String weChatStatus = mSettingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        mViewModel.setWeChatStatus(weChatStatus.equals(SettingController.VALUE_GENERIC_TRUE));
        getSdkVersion();
        mViewModel.setTotalSizeOfDirectories(getTotalSizeOfDirectories());
    }

    @Override
    public void onUpdateSetting(final String key, final boolean value) {
        if (key.equals(SettingController.KEY_SETTING_PRIVACY_STATUS)) {
            mViewModel.updatePrivacyStatus(value);
        }
    }

    /**
     * 退出登录
     */
    public void logoutAccount() {
        mAccountPackage.accountLogoutRequest();
    }

    /**
     * 退出登录后，刷新UI
     */
    @Override
    public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.code == 1) {
            mViewModel.clearUserInfo();
        }
    }

    /**
     * 手机号登录成功，刷新UI
     */
    @Override
    public void notifyMobileLogin(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.code == 1) {
            if (result.profileInfo != null) {
                mViewModel.updateUserInfo(result.profileInfo.nickname, result.profileInfo.avatar);
            }
        }
    }

    @Override
    public void notifyQRCodeLoginConfirm(final int errCode, final int taskId, final AccountUserInfo result) {
        if (result != null && result.code == 1) {
            if (result.profileInfo != null) {
                mViewModel.updateUserInfo(result.profileInfo.nickname, result.profileInfo.avatar);
            }
        }
    }

    public boolean getIsLogin() {
        return mAccountPackage.isLogin();
    }

    @Override
    public void notifyWeixinStatus(final BLResponseBean result) {
        if (result != null) {
            if (result.getCode() == 1) {
                mViewModel.setWeChatStatus(true);
            } else {
                mViewModel.setWeChatStatus(false);
            }
        }
    }

    /**
     * @param result 回调数据
     */
    @Override
    public void notifyGQRCodeConfirm(final BLResponseBean result) {
        if (result != null && result.getCode() == 1) {
            mViewModel.setWeChatStatus(true);
        } else {
            mViewModel.setWeChatStatus(false);
        }
    }

    /**
     * 通过type查找其对应行程历史信息
     * @return size
     */
    public int getValueByType() {
        int size = 0;
        final List<History> list = mHistoryManager.getValueByType("行程历史");
        if (list != null && !list.isEmpty()) {
            size = list.size();
        }
        return size;
    }


    /**
     * 获取SDK版本
     */
    public void getSdkVersion() {
        final String sdkVersion = EnginePackage.getInstance().getSdkVersion();
        mViewModel.setSdkVersion(sdkVersion);
    }

    /**
     * 清除数据库中所有设置相关记录
     */
    public void clearAll() {
        mSettingManager.deleteAll();
    }

    /**
     * @return 获取微信绑定状态
     */
    public boolean getWechatStatus() {
        final String isBind = mSettingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        return isBind.equals(SettingController.VALUE_GENERIC_TRUE);
    }

    /**
     * 删除指定文件夹列表下的所有文件和子文件夹
     */
    public void deleteFilesInDirectories() {
        final File[] dirs = new File[mDirPaths.length];
        for (int i = 0; i < mDirPaths.length; i++) {
            dirs[i] = new File(mDirPaths[i]);
        }
        FileUtils.deleteFilesInDirectories(dirs);
    }

    /**
     * 获取指定文件夹列表下的所有文件的总大小
     * @return 文件夹总大小（字节）
     */
    public String getTotalSizeOfDirectories() {
        final File[] dirs = new File[mDirPaths.length];
        for (int i = 0; i < mDirPaths.length; i++) {
            dirs[i] = new File(mDirPaths[i]);
        }
        return FileUtils.formatFileSize(FileUtils.getTotalSizeOfDirectories(dirs));
    }
}
