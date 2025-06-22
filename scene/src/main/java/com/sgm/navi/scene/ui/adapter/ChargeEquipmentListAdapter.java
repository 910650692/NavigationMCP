package com.sgm.navi.scene.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.ChargeEquipmentItemBinding;
import com.sgm.navi.scene.ui.poi.ChargeStationConfirmDialog;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.search.ConnectorInfoItem;
import com.sgm.navi.service.define.search.EquipmentInfo;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;

public class ChargeEquipmentListAdapter extends RecyclerView.Adapter<ChargeEquipmentListAdapter.equipmentHolder> {
    private ArrayList<EquipmentInfo> equipmentInfoList = new ArrayList<>();
    private Context mContext;
    private OnItemClickListener mItemClickListener;
    private EquipmentInfo mEquipmentInfo;
    private ConnectorInfoItem mConnectorInfoItem;

    public ChargeEquipmentListAdapter(final Context context){
        this.mContext = context;
    }
    @NonNull
    @Override
    public ChargeEquipmentListAdapter.equipmentHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ChargeEquipmentItemBinding chargeEquipmentItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.charge_equipment_item,parent,false);
        return new equipmentHolder(chargeEquipmentItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ChargeEquipmentListAdapter.equipmentHolder holder, int position) {
        final EquipmentInfo info = equipmentInfoList.get(position);
        if(info == null){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "ChargeEquipmentInfo is null");
            return;
        }
        holder.mBinding.setEquipmentInfo(info);
        holder.mBinding.setConnectorInfoItem(info.getmConnectorInfoItem().get(0));
        holder.mBinding.setHandler(new MyEventHandle());

        if(info.getmConnectorInfoItem().get(0).getmParkingLockFlag() == 0 || !"1".equals(info.getmConnectorInfoItem().get(0).getmStatus())){
            holder.mBinding.chargeReservationButton.setAlpha(0.5F);
        }else{
            holder.mBinding.chargeReservationButton.setAlpha(1F);
        }
        holder.mBinding.equipmentUnlock.setText(info.getmConnectorInfoItem().get(0).getmLockStatus() == 10 ? mContext.getString(R.string.charge_lock) : mContext.getString(R.string.charge_unlock));
    }

    @Override
    public int getItemCount() {
        if (equipmentInfoList == null) {
            return 0;
        }
        return equipmentInfoList.size();
    }

    // 更新列表
    public void notifyList(ArrayList<EquipmentInfo> list){
        equipmentInfoList.clear();
        equipmentInfoList.addAll(list);
        notifyDataSetChanged();
    }

    // 打开预约确认弹窗
    public void openReservationDialog(ConnectorInfoItem info,EquipmentInfo equipmentInfo){
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"info: "+info.getmParkingLockFlag()+"--- status: "+info.getmStatus());
        if(info.getmParkingLockFlag() == 0){
            ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.reservation_no_lock));
            return;
        }
        switch (info.getmStatus()){
            case "0":
                ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.equipment_offline));
                return;
            case "2":
            case "3":
                ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.equipment_use));
                return;
            case "5":
                ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.equipment_error));
                return;
        }
        mItemClickListener.onItemClick(info,equipmentInfo);
    }

    // 打开降地锁弹框
    public void openUnLockDialog(ConnectorInfoItem info,EquipmentInfo equipmentInfo){
        new ChargeStationConfirmDialog.Build(mContext).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                mItemClickListener.onUnLockClick(info,equipmentInfo);
            }
        })
        .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.sure_unlock))
        .setTip(ResourceUtils.Companion.getInstance().getString(R.string.sure_unlock_detail))
        .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
        .build().show();
    }

    public void setmItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    public static class equipmentHolder extends RecyclerView.ViewHolder{
        public ChargeEquipmentItemBinding mBinding;
        public equipmentHolder(@NonNull ChargeEquipmentItemBinding binding) {
            super(binding.getRoot());
            this.mBinding = binding;
        }
    }

    public interface OnItemClickListener {
        void onItemClick(ConnectorInfoItem info,EquipmentInfo equipmentInfo);
        void onUnLockClick(ConnectorInfoItem info,EquipmentInfo equipmentInfo);
        void onCancelReservation(ConnectorInfoItem info);
    }

    public class MyEventHandle {
        public void unLock(ConnectorInfoItem item,EquipmentInfo equipmentInfo){
            openUnLockDialog(item,equipmentInfo);
        }

        public void openReservation(ConnectorInfoItem item,EquipmentInfo equipmentInfo){
            openReservationDialog(item,equipmentInfo);
        }
        public void cancelReservation(ConnectorInfoItem item){
            mItemClickListener.onCancelReservation(item);
        }
    }
}
