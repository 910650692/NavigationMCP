package com.sgm.navi.hmi.drivingrecord.adapter;

import android.annotation.SuppressLint;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.DataBindingUtil;
import androidx.databinding.ViewDataBinding;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.TimeUtils;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ItemDrivingRecordBinding;
import com.sgm.navi.hmi.databinding.ItemDrivingRecordEmptyBinding;
import com.sgm.navi.hmi.databinding.ItemDrivingRecordHeaderBinding;
import com.sgm.navi.service.define.user.usertrack.DrivingRecordDataBean;

import java.util.ArrayList;

public class DrivingRecordAdapter extends RecyclerView.Adapter<DrivingRecordAdapter.MultiHolder> {

    public final static int DRIVE_RECORD_HEADER = 1;
    public final static int DRIVE_RECORD_EMPTY = 2;
    private ArrayList<DrivingRecordDataBean> mDataList = new ArrayList();
    private OnItemClickListener mItemClickListener;
    private OnItemOnCreateView mOnItemOnCreateView;
    private ItemDrivingRecordHeaderBinding mDrivingRecordHeaderBinding;
    private ItemDrivingRecordEmptyBinding mDrivingRecordEmptyBinding;

    public DrivingRecordAdapter() {
    }

    /**
     * 设置数据
     *
     * @param drivingRecordList 行程数据
     */
    public void setDrivingRecordList(final ArrayList<DrivingRecordDataBean> drivingRecordList) {
        this.mDataList.clear();
        this.mDataList.addAll(drivingRecordList);
        notifyDataSetChanged();
    }

    /**
     * 设置点击事件
     *
     * @param itemClickListener 点击事件
     */
    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    /**
     * 获取头布局item
     * @return
     */
    @Nullable
    public ItemDrivingRecordHeaderBinding getItemHeaderBinding() {
        return mDrivingRecordHeaderBinding;
    }

    /**
     * 设置item view 创建事件
     *
     * @param onItemOnCreateView 回调事件
     */
    public void setItemOnCreateViewListener(final OnItemOnCreateView onItemOnCreateView) {
        this.mOnItemOnCreateView = onItemOnCreateView;
    }

    @Override
    public MultiHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        if (viewType == DRIVE_RECORD_HEADER) {
            mDrivingRecordHeaderBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                R.layout.item_driving_record_header, parent, false);
            if (mOnItemOnCreateView != null) {
                mOnItemOnCreateView.onCreateHeaderView(mDrivingRecordHeaderBinding);
            }
            return new MultiHolder(mDrivingRecordHeaderBinding, viewType);
        } else if (viewType == DRIVE_RECORD_EMPTY) {
            mDrivingRecordEmptyBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                R.layout.item_driving_record_empty, parent, false);
            if (mOnItemOnCreateView != null) {
                mOnItemOnCreateView.onCreateEmptyView(mDrivingRecordEmptyBinding);
            }
            return new MultiHolder(mDrivingRecordEmptyBinding, viewType);
        } else {
            final ItemDrivingRecordBinding drivingRecordBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                R.layout.item_driving_record, parent, false);
            return new Holder(drivingRecordBinding);
        }
    }

    @Override
    public int getItemCount() {
        if (mDataList.isEmpty()) {
            return 2;
        }
        return mDataList.size() + 1;
    }

    @Override
    public int getItemViewType(int position) {
        if (position == 0) {
            return DRIVE_RECORD_HEADER;
        } else if (position == 1) {
            return mDataList.isEmpty() ? DRIVE_RECORD_EMPTY : super.getItemViewType(position);
        } else {
            return super.getItemViewType(position);
        }
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull final MultiHolder multiHolder, final int position) {
        int viewType = multiHolder.viewType;
        if (viewType == DRIVE_RECORD_HEADER) {
            if (mOnItemOnCreateView != null) {
                mOnItemOnCreateView.onResumeHeaderView((ItemDrivingRecordHeaderBinding) multiHolder.binding);
            }
        } else if (viewType == DRIVE_RECORD_EMPTY) {
            if (mOnItemOnCreateView != null) {
                mOnItemOnCreateView.onResumeEmptyView((ItemDrivingRecordEmptyBinding) multiHolder.binding);
            }
        } else {
            Holder holder = (Holder) multiHolder;
            int subPosition = position - 1;
            holder.binding.setModel(mDataList.get(subPosition));

            // 最后一条数据下不显示分割线
            if (subPosition == mDataList.size() - 1) {
                holder.binding.viewLine.setVisibility(View.GONE);
            } else {
                holder.binding.viewLine.setVisibility(View.VISIBLE);
            }

            // 设置起点
            holder.binding.itemDrivingStartName.setText(TextUtils.isEmpty(mDataList.get(subPosition).getStartPoiName())
                ? "--" : mDataList.get(subPosition).getStartPoiName());
            // 设置终点
            holder.binding.itemDrivingEndName.setText(TextUtils.isEmpty(mDataList.get(subPosition).getEndPoiName())
                ? "--" : mDataList.get(subPosition).getEndPoiName());
            // 该行程行驶距离
            holder.binding.itemDrivingDistance.setText(TimeUtils.getInstance().getDistanceString(mDataList.get(subPosition).getRunDistance()));
            // 该行程完成时间
            holder.binding.itemDrivingDate.setText(mDataList.get(subPosition).getStartTime());
            //删除删除item
            holder.binding.itemDrivingDelete.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemDeleteClick(subPosition);
                }
            });
            // 查看行程详情
            holder.binding.itemView.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemClick(subPosition);
                }
            });
        }
    }

    public class Holder extends MultiHolder<ItemDrivingRecordBinding> {
        public Holder(final ItemDrivingRecordBinding drivingRecordBinding) {
            super(drivingRecordBinding, 0);
            drivingRecordBinding.setHolder(this);
        }
    }

    public static class MultiHolder<T extends ViewDataBinding> extends RecyclerView.ViewHolder {
        public int viewType;
        public T binding;

        public MultiHolder(final T binding, int viewType) {
            super(binding.getRoot());
            this.binding = binding;
            this.viewType = viewType;
        }
    }


    public interface OnItemClickListener {

        /**
         * 行程详情点击事件
         *
         * @param index 点击的行程索引
         */
        void onItemClick(int index);

        /**
         * 行程删除点击事件
         *
         * @param index 点击的行程索引
         */
        void onItemDeleteClick(int index);
    }

    public interface OnItemOnCreateView {
        /**
         * 头布局 创建监听回调
         *
         * @param itemDrivingRecordHeaderBinding 布局文件
         */
        default void onCreateHeaderView(ItemDrivingRecordHeaderBinding itemDrivingRecordHeaderBinding) {
        }

        /**
         * 头布局 显示监听回调
         *
         * @param itemDrivingRecordHeaderBinding 布局文件
         */
        default void onResumeHeaderView(ItemDrivingRecordHeaderBinding itemDrivingRecordHeaderBinding) {
        }

        /**
         * 空布局 创建监听回调
         *
         * @param itemDrivingRecordEmptyBinding 布局文件
         */
        default void onCreateEmptyView(ItemDrivingRecordEmptyBinding itemDrivingRecordEmptyBinding) {
        }

        /**
         * 空布局 显示监听回调
         *
         * @param itemDrivingRecordEmptyBinding 布局文件
         */
        default void onResumeEmptyView(ItemDrivingRecordEmptyBinding itemDrivingRecordEmptyBinding) {
        }

    }
}