package com.sgm.navi.hmi.drivingrecord.adapter;

import android.annotation.SuppressLint;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.TimeUtils;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ItemDrivingRecordBinding;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;

import java.util.ArrayList;

public class DrivingRecordAdapter extends RecyclerView.Adapter<DrivingRecordAdapter.Holder> {
    private ArrayList<DrivingRecordDataBean> mDataList = new ArrayList();
    private OnItemClickListener mItemClickListener;

    public DrivingRecordAdapter() {
    }

    /**
     * 设置数据
     * @param drivingRecordList 行程数据
     */
    public void setDrivingRecordList(final ArrayList<DrivingRecordDataBean> drivingRecordList) {
        this.mDataList = drivingRecordList;
        notifyDataSetChanged();
    }

    /**
     * 设置点击事件
     * @param itemClickListener 点击事件
     */
    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final ItemDrivingRecordBinding drivingRecordBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_driving_record, parent, false);
        return new Holder(drivingRecordBinding);
    }

    @Override
    public int getItemCount() {
        if (mDataList == null) {
            return 0;
        }
        return mDataList.size();
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mDrivingRecordBinding.setModel(mDataList.get(position));

        // 最后一条数据下不显示分割线
        if (position == mDataList.size() - 1) {
            holder.mDrivingRecordBinding.viewLine.setVisibility(View.GONE);
        } else {
            holder.mDrivingRecordBinding.viewLine.setVisibility(View.VISIBLE);
        }

        // 设置起点
        holder.mDrivingRecordBinding.itemDrivingStartName.setText(TextUtils.isEmpty(mDataList.get(position).getStartPoiName())
                ? "--" : mDataList.get(position).getStartPoiName());
        // 设置终点
        holder.mDrivingRecordBinding.itemDrivingEndName.setText(TextUtils.isEmpty(mDataList.get(position).getEndPoiName())
                ? "--" : mDataList.get(position).getEndPoiName());
        // 该行程行驶距离
        holder.mDrivingRecordBinding.itemDrivingDistance.setText(TimeUtils.getInstance().getDistanceString(mDataList.get(position).getRunDistance()));
        // 该行程完成时间
        holder.mDrivingRecordBinding.itemDrivingDate.setText(mDataList.get(position).getStartTime());
        //删除删除item
        holder.mDrivingRecordBinding.itemDrivingDelete.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemDeleteClick(position);
            }
        });
        // 查看行程详情
        holder.mDrivingRecordBinding.itemView.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(position);
            }
        });
    }

    public class Holder extends RecyclerView.ViewHolder {
        final private ItemDrivingRecordBinding mDrivingRecordBinding;

        public Holder(final ItemDrivingRecordBinding drivingRecordBinding) {
            super(drivingRecordBinding.getRoot());
            this.mDrivingRecordBinding = drivingRecordBinding;
            drivingRecordBinding.setHolder(this);
        }
    }


    public interface OnItemClickListener {

        /**
         * 行程详情点击事件
         * @param index 点击的行程索引
         */
        void onItemClick(int index);

        /**
         * 行程删除点击事件
         * @param index 点击的行程索引
         */
        void onItemDeleteClick(int index);
    }
}