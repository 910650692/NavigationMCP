package com.sgm.navi.hmi.wechat;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.text.TextUtils;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.user.wechat.BLResponseBean;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.user.wechat.WeChatCallBack;
import com.sgm.navi.service.logicpaket.user.wechat.WeChatPackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.Base64;
import java.util.Objects;


public class WeChatModel extends BaseModel<WeChatViewModel> implements WeChatCallBack {

    private static final String TAG = MapDefaultFinalTag.WECHAT_HMI_TAG;
    private final WeChatPackage mWeChatPackage;
    private final SettingManager mSettingManager;

    public WeChatModel() {
        mWeChatPackage = WeChatPackage.getInstance();
        mWeChatPackage.initWeChatService();
        mSettingManager = new SettingManager();
        mSettingManager.init();
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mWeChatPackage.registerCallBack("WeChatModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mWeChatPackage.unRegisterCallBack("WeChatModel");
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
    }

    @Override
    public void notifyGQRCodeConfirm(final BLResponseBean result) {
        if (result != null && result.getCode() == 1) {
            Logger.d(TAG, "notifyGQRCodeConfirm = " + GsonUtils.toJson(result));
            mViewModel.setIsBind(true);
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_TRUE);
        }
    }

    @Override
    public void notifyWeixinQrcode(final BLResponseBean result) {
        mViewModel.stopAnimation();
        if (result != null && result.getCode() == 1) {
            Logger.d(TAG, "notifyWeixinQrcode = " + GsonUtils.toJson(result));
            final byte[] imageBytes = Base64.getDecoder().decode(result.getImgStr());
            final Bitmap bitmap = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.length);
            mViewModel.updateQRCode(bitmap);
            mViewModel.updateLoadingVisible(false, false, true);
        } else {
            if (result != null) {
                switch (result.getCode()) {
                    case 2:
                        ToastUtils.Companion.getInstance().showCustomToastView("访问失败");
                        break;
                    case 14:
                        ToastUtils.Companion.getInstance().showCustomToastView("登录状态失效，请重新登录");
                        break;
                    default:
                        break;
                }
            }
            mViewModel.updateLoadingVisible(false, true, false);
        }
    }

    private int resultCode;
    @Override
    public void notifyWeixinStatus(final BLResponseBean result) {
        if (result != null) {
            Logger.d(TAG, "notifyWeixinStatus = " + GsonUtils.toJson(result));
            if (resultCode == result.getCode()) {
                return;
            }
            resultCode = result.getCode();
            if (result.getCode() == 14 || result.getCode() == 10060) {
                mViewModel.setIsBind(false);
                mViewModel.startAnimation();
                mViewModel.updateLoadingVisible(true, false, false);
                mWeChatPackage.sendReqWsPpAutoWeixinQrcode();
                mSettingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_FALSE);
            } else if (result.getCode() == 1) {
                mViewModel.setIsBind(true);
                mSettingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_TRUE);
            }
        } else {
            Logger.d(TAG, "notifyWeixinStatus result == null");
        }
    }

    /**
     * 发送请求获取二维码
     */
    public void sendReqWsPpAutoWeixinQrcode() {
        mWeChatPackage.sendReqWsPpAutoWeixinQrcode();
    }

    /**
     * 发送请求获取绑定状态
     */
    public void getBindStatus() {
        final String isBind = mSettingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        if (Objects.equals(isBind, SettingController.VALUE_GENERIC_TRUE)) {
            mViewModel.setIsBind(true);
        } else if (isBind == null){
            mViewModel.setIsBind(false);
        } else {
            if (!getNetworkState()) {
                ToastUtils.Companion.getInstance().showCustomToastView(
                        ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
                mViewModel.updateLoadingVisible(false, true, false);
                return;
            }
            mViewModel.setIsBind(false);
        }
        mWeChatPackage.sendReqWsPpAutoWeixinStatus();
    }

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {

        }

        @Override
        public void onNetDisConnect() {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.setting_qr_code_load_offline_toast));
            mViewModel.stopAnimation();
            mViewModel.updateLoadingVisible(false, true, false);
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };

    /**
     * 获取网络状态
     * @return 网络状态
     */
    public boolean getNetworkState() {
        return Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
    }
}
