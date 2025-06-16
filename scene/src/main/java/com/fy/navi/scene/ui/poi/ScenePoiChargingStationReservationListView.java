package com.fy.navi.scene.ui.poi;

import android.annotation.SuppressLint;
import android.app.Activity;
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
import com.fy.navi.scene.dialog.MsgTopDialog;
import com.fy.navi.scene.impl.poi.ScenePoiChargingStationReservationListViewImpl;
import com.fy.navi.scene.ui.adapter.ChargeEquipmentListAdapter;
import com.fy.navi.scene.ui.search.ChargeQrCodeDialog;
import com.fy.navi.scene.ui.search.SearchConfirmDialog;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargePriceInfo;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.ReservationInfo;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.define.TripID;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.sql.Time;
import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import java.util.stream.Collectors;

public class ScenePoiChargingStationReservationListView extends BaseSceneView<ScenePoiDetailsReservationChargeDetailViewBinding, ScenePoiChargingStationReservationListViewImpl> {
    private static final String TAG = ScenePoiChargingStationReservationListView.class.getSimpleName();
    private ChargeEquipmentListAdapter mAdapter;
    private ArrayList<EquipmentInfo> mEquipmentList;
    private PoiInfoEntity mPoiInfoEntity;
    private EquipmentInfo mCurrentEquipmentInfo;
    private ConnectorInfoItem mCurrentConnector;
    private final int mSpanCount = 2;
    private Integer mCancelNumber = 0;
    private static final Integer MAX_CANCEL = 3;
    private Integer mCurrentType = 1;
    private AccessTokenParam mParams;

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
        mViewBinding.setEventHandle(new MyEventHandle());
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
            public void onItemClick(ConnectorInfoItem info,EquipmentInfo equipmentInfo) {
                mCurrentEquipmentInfo = equipmentInfo;
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"Cancel Time: "+mCancelNumber);
                if(mCancelNumber >= MAX_CANCEL){
                    ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.reservation_max_time));
                    return;
                }
                new ChargeStationConfirmDialog.Build(getContext()).setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        // 预约行为
                        mScreenViewModel.createReversion(info,mPoiInfoEntity,mParams);
                    }
                })
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_title))
                .setTip(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip))
                .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip_confirm))
                .build().show();
            }

            @Override
            public void onUnLockClick(ConnectorInfoItem info,EquipmentInfo equipmentInfo) {
                mCurrentConnector = info;
                mScreenViewModel.unGroundLock(info,mPoiInfoEntity,mParams);
            }

            @Override
            public void onCancelReservation(ConnectorInfoItem info) {
                new ChargeStationConfirmDialog.Build(getContext()).setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mScreenViewModel.queryReservation(mPoiInfoEntity,mParams,1);
                    }
                })
                .setTitle(getContext().getString(R.string.cancel_num_tip,mCancelNumber.toString()))
                .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
                .build().show();
            }
        });
        mViewBinding.poiChargeStationList.setAdapter(mAdapter);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mParams = getAccessTokenParam((Activity) getContext());
    }

    public void notifyEquipmentInfo(int type, PoiInfoEntity poiInfoEntity){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"notifyEquipmentInfo type: "+type);

        mScreenViewModel.mPoiInfo.setValue(poiInfoEntity);
        mCurrentType = type;
        mScreenViewModel.searchType.setValue(type);
        if(!ConvertUtils.isEmpty(poiInfoEntity.getChargeInfoList()) && !poiInfoEntity.getChargeInfoList().isEmpty()){
            mScreenViewModel.mChargeInfo.setValue(poiInfoEntity.getChargeInfoList().get(0));
        }else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiInfoEntity.getChargeInfoList is null");
        }
        mPoiInfoEntity = poiInfoEntity;
        mEquipmentList = poiInfoEntity.getChargeInfoList().get(0).getEquipmentInfo();
        ArrayList<EquipmentInfo> filterEquipmentInfo = filterEquipmentList(type);
        mAdapter.notifyList(filterEquipmentInfo);
        mScreenViewModel.queryReservation(mPoiInfoEntity,mParams,3);
    }

    // 关闭当前页面
    public void closeFragment(){
        closeCurrentFragment();
    }

    public void notifyCreateReservationSuccess(int taskId){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        mScreenViewModel.queryEquipmentInfo(mCurrentEquipmentInfo,mPoiInfoEntity);
        // 开启预约倒计时
        GeoPoint point = new GeoPoint();
        point.setLon(mPoiInfoEntity.getPoint().getLon());
        point.setLat(mPoiInfoEntity.getPoint().getLat());
        mScreenViewModel.createTimeTick(point);
    }

    public void onSearchError(int taskId,String message){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"message: "+message+"--taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        ToastUtils.Companion.getInstance().showCustomToastView(getContext().getString(R.string.load_failed));
    }

    public void notifyEquipmentResult(int taskId,EquipmentInfo info){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        if(!ConvertUtils.isEmpty(mEquipmentList)){
            for (int i = 0; i < mEquipmentList.size(); i++) {
                if(info.getmEquipmentId().equals(mEquipmentList.get(i).getmEquipmentId())){
                    mEquipmentList.set(i,info);
                }
            }
        }
        ArrayList<EquipmentInfo> filterEquipmentInfo = filterEquipmentList(mCurrentType);
        mAdapter.notifyList(filterEquipmentInfo);
    }

    public void notifyUnLockResult(int taskId){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        mScreenViewModel.queryEquipmentInfo(mCurrentEquipmentInfo,mPoiInfoEntity);
    }

    public void notifyCancelSuccess(int taskId){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        mScreenViewModel.queryEquipmentInfo(mCurrentEquipmentInfo,mPoiInfoEntity);
    }

    public void notifyCancelReservation(int taskId,ArrayList<ReservationInfo> list){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        mCancelNumber = list.size();
    }

    public void notifyReadyReservation(int taskId,ArrayList<ReservationInfo> list){
        if(ConvertUtils.isNull(mScreenViewModel)){
            return;
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"taskId: "+taskId + "--currentTaskId: "+mScreenViewModel.getMTaskId());
        if ((!ConvertUtils.equals(taskId, mScreenViewModel.getMTaskId()) && mScreenViewModel.getMTaskId() != 0) || mViewBinding == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"error");
            return;
        }
        if(!ConvertUtils.isEmpty(list)){
            String id = AccountPackage.getInstance().getUserId();
            for (int i = 0; i < list.size(); i++) {
                if(id.equals(list.get(i).getmUserId()) && 1 == list.get(i).getmStatus()){
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"Cancel Pre");
                    mScreenViewModel.cancelReservation(list.get(i),mParams);
                }
            }
        }
    }

    private ArrayList<EquipmentInfo> filterEquipmentList(int type){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"type: "+type);
        ArrayList<EquipmentInfo> equipmentInfos = new ArrayList<>();
        return switch (type) {
            case AutoMapConstant.EquipmentType.FAST, AutoMapConstant.EquipmentType.SLOW -> {
                equipmentInfos = mEquipmentList.stream().filter(equipmentInfo ->
                        String.valueOf(type).equals(equipmentInfo.getmConnectorInfoItem().get(0).getmChargeType())
                ).collect(Collectors.toCollection(ArrayList::new));
                yield equipmentInfos;
            }
            case AutoMapConstant.EquipmentType.RESERVATION -> {
                equipmentInfos = mEquipmentList.stream().filter(equipmentInfo ->
                        "1".equals(equipmentInfo.getmConnectorInfoItem().get(0).getmStatus())
                ).collect(Collectors.toCollection(ArrayList::new));
                yield equipmentInfos;
            }
            default -> mEquipmentList;
        };
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    private void clearClickBg(){
        mViewBinding.poiFastInfo.setBackground(getContext().getDrawable(R.drawable.bg_info_normal));
        mViewBinding.poiReservation.setBackground(getContext().getDrawable(R.drawable.bg_info_normal));
        mViewBinding.poiSlowInfo.setBackground(getContext().getDrawable(R.drawable.bg_info_normal));

        mViewBinding.poiFastInfo.setTextColor(getContext().getColor(R.color.text_route_defult));
        mViewBinding.poiReservation.setTextColor(getContext().getColor(R.color.text_route_defult));
        mViewBinding.poiSlowInfo.setTextColor(getContext().getColor(R.color.text_route_defult));
    }

    private AccessTokenParam getAccessTokenParam(Activity activity){
        if(!ConvertUtils.isNull(activity)) return null;
        return new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
    }

    public class MyEventHandle {
        public void openChargeQrCodeDialog(){
            new ChargeQrCodeDialog.Build(getContext()).build().show();
        }
        @SuppressLint("UseCompatLoadingForDrawables")
        public void filterEquipment(int type){
            clearClickBg();
            mCurrentType = type;
            if(type == 0){
                mViewBinding.poiSlowInfo.setBackground(getContext().getDrawable(R.drawable.bg_info_select));
                mViewBinding.poiSlowInfo.setTextColor(getContext().getColor(R.color.navi_color_2461EA_100));
            }else if(type == 1){
                mViewBinding.poiFastInfo.setBackground(getContext().getDrawable(R.drawable.bg_info_select));
                mViewBinding.poiFastInfo.setTextColor(getContext().getColor(R.color.navi_color_2461EA_100));
            }else if(type == 2){
                mViewBinding.poiReservation.setBackground(getContext().getDrawable(R.drawable.bg_info_select));
                mViewBinding.poiReservation.setTextColor(getContext().getColor(R.color.navi_color_2461EA_100));
            }
            ArrayList<EquipmentInfo> filterEquipmentInfo = filterEquipmentList(type);
            mAdapter.notifyList(filterEquipmentInfo);
        }
    }
}
