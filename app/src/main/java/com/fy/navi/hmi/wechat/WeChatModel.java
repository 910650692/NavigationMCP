package com.fy.navi.hmi.wechat;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.wechat.BLResponseBean;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.user.wechat.WeChatCallBack;
import com.fy.navi.service.logicpaket.user.wechat.WeChatPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.Base64;
import java.util.Objects;

/**
 * @Description TODO
 * @Author fh
 * @date 2024/12/18
 */
public class WeChatModel extends BaseModel<WeChatViewModel> implements WeChatCallBack {

    private static final String TAG = MapDefaultFinalTag.WECHAT_HMI_TAG;
    private final WeChatPackage weChatPackage;
    private final SettingManager settingManager;

    public WeChatModel() {
        weChatPackage = WeChatPackage.getInstance();
        weChatPackage.initWeChatService();
        settingManager = new SettingManager();
        settingManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        weChatPackage.registerCallBack("WeChatModel",this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        weChatPackage.unRegisterCallBack("WeChatModel");
    }

    @Override
    public void notifyGQRCodeConfirm(BLResponseBean result) {
        if (result != null && result.getCode() == 1) {
            Logger.d(TAG, "notifyGQRCodeConfirm = " + GsonUtils.toJson(result));
            mViewModel.setIsBind(true);
            settingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_TRUE);
        }
    }

    @Override
    public void notifyWeixinQrcode(BLResponseBean result) {
        if (result != null && result.getCode() == 1) {
            Logger.d(TAG, "notifyWeixinQrcode = " + GsonUtils.toJson(result));
            byte[] imageBytes = Base64.getDecoder().decode(result.getImgStr());
            Bitmap bitmap = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.length);
            mViewModel.updateQRCode(bitmap);
        }
    }

    @Override
    public void notifyWeixinStatus(BLResponseBean result) {
        if (result != null) {
            Logger.d(TAG,"notifyWeixinStatus = " + GsonUtils.toJson(result));
            if (result.getCode() == 14 || result.getCode() == 10060) {
                mViewModel.setIsBind(false);
                weChatPackage.sendReqWsPpAutoWeixinQrcode();
                settingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_FALSE);
            } else if (result.getCode() == 1) {
                mViewModel.setIsBind(true);
                settingManager.insertOrReplace(SettingController.KEY_SETTING_IS_WE_CHAT_BIND, SettingController.VALUE_GENERIC_TRUE);
            }
        }
    }

    public void getBindStatus() {
        String isBind = settingManager.getValueByKey(SettingController.KEY_SETTING_IS_WE_CHAT_BIND);
        if (Objects.equals(isBind, SettingController.VALUE_GENERIC_TRUE)) {
            mViewModel.setIsBind(true);
        } else if (isBind == null){
            mViewModel.setIsBind(false);
            weChatPackage.sendReqWsPpAutoWeixinStatus();
        } else {
            mViewModel.setIsBind(false);
            weChatPackage.sendReqWsPpAutoWeixinQrcode();
        }
        weChatPackage.sendReqWsPpAutoWeixinStatus();
    }
}
