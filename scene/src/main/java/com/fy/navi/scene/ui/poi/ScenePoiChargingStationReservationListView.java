package com.fy.navi.scene.ui.poi;

import android.content.Context;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ScenePoiDetailsReservationChargeDetailViewBinding;
import com.fy.navi.scene.impl.poi.ScenePoiChargingStationReservationListViewImpl;
import com.fy.navi.scene.ui.adapter.ChargeEquipmentListAdapter;
import com.fy.navi.scene.ui.search.ChargeQrCodeDialog;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargePriceInfo;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.sql.Time;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public class ScenePoiChargingStationReservationListView extends BaseSceneView<ScenePoiDetailsReservationChargeDetailViewBinding, ScenePoiChargingStationReservationListViewImpl> {
    private static final String TAG = ScenePoiChargingStationReservationListView.class.getSimpleName();
    private ChargeEquipmentListAdapter mAdapter;
    private ArrayList<EquipmentInfo> mEquipmentList;
    private ConnectorInfoItem mCurrentConnector;
    private final int mSpanCount = 2;

    public ScenePoiChargingStationReservationListView(@NonNull Context context) {
        super(context);
    }

    public ScenePoiChargingStationReservationListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public ScenePoiChargingStationReservationListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected ScenePoiDetailsReservationChargeDetailViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return ScenePoiDetailsReservationChargeDetailViewBinding.inflate(inflater,viewGroup,true);
    }

    @Override
    protected ScenePoiChargingStationReservationListViewImpl initSceneImpl() {
        return new ScenePoiChargingStationReservationListViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScenePoiChargingStationReservationListView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        initAdapter();
    }

    private void initAdapter(){
        final int spacingInPixels = getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_16);
        final GridLayoutManager layoutManager = new GridLayoutManager(getContext(),mSpanCount);
        mViewBinding.poiChargeStationList.setLayoutManager(layoutManager);
        mViewBinding.poiChargeStationList.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(@NonNull Rect outRect, @NonNull View view, @NonNull RecyclerView parent, @NonNull RecyclerView.State state) {
                outRect.bottom = spacingInPixels;
            }
        });
        mAdapter = new ChargeEquipmentListAdapter(getContext());
        mAdapter.setmItemClickListener(new ChargeEquipmentListAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(ConnectorInfoItem info) {
                mScreenViewModel.createReversion();
            }

            @Override
            public void onUnLockClick(ConnectorInfoItem info) {
                mCurrentConnector = info;
                mScreenViewModel.unGroundLock();
            }

            @Override
            public void onCancelReservation(ConnectorInfoItem info) {

            }
        });
        mViewBinding.poiChargeStationList.setAdapter(mAdapter);
    }

    public void notifyEquipmentInfo(int type, PoiInfoEntity poiInfoEntity){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"notifyEquipmentInfo type: "+type);
        mScreenViewModel.searchType.setValue(type);
        mScreenViewModel.mPoiInfo.setValue(poiInfoEntity);
        if(!ConvertUtils.isEmpty(poiInfoEntity.getChargeInfoList()) && !poiInfoEntity.getChargeInfoList().isEmpty()){
            mScreenViewModel.mChargeInfo.setValue(poiInfoEntity.getChargeInfoList().get(0));
        }else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiInfoEntity.getChargeInfoList is null");
        }
        mEquipmentList = poiInfoEntity.getChargeInfoList().get(0).getEquipmentInfo();
        mAdapter.notifyList(mEquipmentList);
    }

    // 关闭当前页面
    public void closeFragment(){
        closeCurrentFragment();
    }

    // 打开qccode弹窗
    public void openChargeQrCodeDialog(){
        new ChargeQrCodeDialog.Build(getContext()).build().show();
    }

    public void notifyCreateReservationSuccess(){
        mScreenViewModel.queryEquipmentInfo();
    }

    public void notifyEquipmentResult(EquipmentInfo info){
        if(!ConvertUtils.isEmpty(mEquipmentList)){
            for (int i = 0; i < mEquipmentList.size(); i++) {
                if(info.getmEquipmentId().equals(mEquipmentList.get(i).getmEquipmentId())){
                    mEquipmentList.set(i,info);
                }
            }
        }
        mAdapter.notifyList(mEquipmentList);
    }

    public void notifyUnLockResult(String code){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"code: "+code);
        if("0000".equals(code)){

        }
//        ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString( ? R.string.unlock_tip_success : R.string.unlock_tip_fail));
    }
}
