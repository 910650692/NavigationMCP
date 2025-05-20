package com.fy.navi.scene.ui.adapter;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.ChargeEquipmentItemBinding;
import com.fy.navi.scene.ui.poi.ChargeStationConfirmDialog;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.search.ChargeEquipmentInfo;
import com.fy.navi.service.define.search.ChargePriceInfo;
import com.fy.navi.service.define.search.ConnectorInfoItem;
import com.fy.navi.service.define.search.EquipmentInfo;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;

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
        mEquipmentInfo = info;
        holder.mBinding.setConnectorInfoItem(info.getmConnectorInfoItem().get(0));
        holder.mBinding.setHandler(new MyEventHandle());

        mConnectorInfoItem = info.getmConnectorInfoItem().get(0);
        if(info.getmConnectorInfoItem().get(0).getmParkingLockFlag() == 0 || !"1".equals(info.getmConnectorInfoItem().get(0).getmStatus())){
            holder.mBinding.chargeReservationButton.setEnabled(false);
            holder.mBinding.chargeReservationButton.setAlpha(0.5F);
        }else{
            holder.mBinding.chargeReservationButton.setEnabled(true);
            holder.mBinding.chargeReservationButton.setAlpha(1F);
        }
        holder.mBinding.equipmentUnlock.setText(mConnectorInfoItem.getmLockStatus() == 10 ? mContext.getString(R.string.charge_lock) : mContext.getString(R.string.charge_unlock));
        holder.mBinding.chargeReservationButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if(mConnectorInfoItem.getmParkingLockFlag() == 0 || !"1".equals(mConnectorInfoItem.getmStatus())){
                    return;
                }
                openReservationDialog(mConnectorInfoItem,info);
            }
        });
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
        if(info.getmParkingLockFlag() == 0){
            ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.reservation_no_lock));
            return;
        }
        switch (info.getmStatus()){
            case "2":
            case "3":
                ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.equipment_use));
                return;
            case "5":
                ToastUtils.Companion.getInstance().showCustomToastView(mContext.getString(R.string.equipment_error));
                return;
        }
        new ChargeStationConfirmDialog.Build(mContext).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                // 预约行为
                mItemClickListener.onItemClick(info,equipmentInfo);
            }
        })
        .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_title))
        .setTip(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip))
        .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.reservation_tip_confirm))
        .build().show();
    }

    // 打开降地锁弹框
    public void openUnLockDialog(ConnectorInfoItem info){
        new ChargeStationConfirmDialog.Build(mContext).setDialogObserver(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                mItemClickListener.onUnLockClick(info,mEquipmentInfo);
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
        public void unLock(ConnectorInfoItem item){
            openUnLockDialog(item);
        }
        public void cancelReservation(ConnectorInfoItem item){
            mItemClickListener.onCancelReservation(item);
        }
    }
}
