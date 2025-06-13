package com.fy.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.DialogForecastAddressBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.fy.navi.service.logicpaket.user.forecast.IForecastAddressCallBack;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;


public class ForecastAddressDialog extends BaseFullScreenDialog<DialogForecastAddressBinding> {

    private IForecastAddressCallBack mIForecastAddressCallBack;
    private int mType;
    private OftenArrivedItemInfo mOftenArrivedItemInfo;
    public ForecastAddressDialog(final Context context, final int type, final OftenArrivedItemInfo oftenArrivedItemInfo,
                                 final IForecastAddressCallBack addressCallBack) {
        super(context);
        this.mType = type;
        this.mOftenArrivedItemInfo = oftenArrivedItemInfo;
        this.mIForecastAddressCallBack = addressCallBack;
    }

    @Override
    protected DialogForecastAddressBinding initLayout() {
        return DialogForecastAddressBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setDialogView();

        if(!ConvertUtils.isEmpty(mOftenArrivedItemInfo.getWstrAddress())){
            mViewBinding.forecastContent.setText(mOftenArrivedItemInfo.getWstrAddress());
        }
        if(mType == AutoMapConstant.HomeCompanyType.HOME){
            mViewBinding.forecastTitle.setText(ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home));
        }else {
            mViewBinding.forecastTitle.setText(ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_company));
        }

        mViewBinding.dialogCommit.setOnClickListener(v -> {
            onDismiss();
            //设置数据
            mIForecastAddressCallBack.AddForecastInfo(mOftenArrivedItemInfo);

            if(mViewBinding.forecastTitle.getText() == ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home)){
                sendBuryPointForPrediction(BuryConstant.Number.ONE);
            }
        });

        mViewBinding.dialogCancel.setOnClickListener(v -> {
            onDismiss();

            if(mViewBinding.forecastTitle.getText() == ResourceUtils.Companion.getInstance().getText(R.string.forecast_title_home)){
                sendBuryPointForPrediction(BuryConstant.Number.SECOND);
            }
        });

        mViewBinding.forecastContent.setOnClickListener(v -> {
            onDismiss();
            mIForecastAddressCallBack.addressClick();
        });

        mViewBinding.forecastEdit.setOnClickListener(v -> {
            onDismiss();
            mViewBinding.forecastContent.callOnClick();
        });
    }

    private void setDialogView() {
        if(BuildConfig.FLAVOR.equals("clea_local_8155")){
            mViewBinding.forecastContent.setCompoundDrawablesWithIntrinsicBounds(null,null,null,null);
            mViewBinding.forecastEdit.setVisibility(View.VISIBLE);
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_HOME_PREDICTION)
    private void sendBuryPointForPrediction(String number){
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, number)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }

    /**
     * dismiss dialog
     */
    private void onDismiss(){
        dismiss();
    }
}
