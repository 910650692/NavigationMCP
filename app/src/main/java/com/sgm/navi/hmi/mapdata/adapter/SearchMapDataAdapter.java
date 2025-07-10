package com.sgm.navi.hmi.mapdata.adapter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.utils.StringUtils;
import com.sgm.navi.scene.ui.setting.DownloadBtnView;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.mapdata.CityDataInfo;
import com.sgm.navi.service.define.mapdata.CityDownLoadInfo;
import com.sgm.navi.service.define.mapdata.ProvDataInfo;
import com.sgm.navi.ui.view.CustomSwipeMenuLayout;
import com.sgm.navi.ui.view.SkinButton;
import com.sgm.navi.ui.view.SkinImageView;
import com.sgm.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SearchMapDataAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    public static final String TAG = SearchMapDataAdapter.class.getName();
    private static final int TYPE_PARENT = 0;
    private static final int TYPE_CHILD = 1;
    private Context mContext;
    private final List<ProvDataInfo> mProvinceList = new ArrayList<>();
    private OnChildClickListener mOnChildClickListener;
    private boolean mIsClickAllStart = true;
    private boolean mIsClickAllPause = false;

    public SearchMapDataAdapter(final Context context) {
        this.mContext = context;
    }

    /**
     * set adapter data
     * @param parentItems
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setData(final List<ProvDataInfo> parentItems) {
        if (parentItems != null) {
            mProvinceList.clear();
            mProvinceList.addAll(parentItems);
            notifyDataSetChanged();
        }
    }

    public void setOnChildClickListener(final OnChildClickListener listener) {
        this.mOnChildClickListener = listener;
    }

    @Override
    public int getItemViewType(final int position) {
        int itemCount = 0;
        for (ProvDataInfo provDataInfo : mProvinceList) {
            if (position == itemCount) {
                return provDataInfo.isParentCity() ? TYPE_CHILD : TYPE_PARENT;
            }
            itemCount++;
            if (provDataInfo.isExpanded()) {
                if (position < itemCount + provDataInfo.getCityInfoList().size()) {
                    return TYPE_CHILD;
                }
                itemCount += provDataInfo.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(final ViewGroup parent, final int viewType) {
        final LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        if (viewType == TYPE_PARENT) {
            final View view = inflater.inflate(R.layout.item_province, parent, false);
            return new ParentViewHolder(view);
        } else {
            final View view = inflater.inflate(R.layout.item_city_data, parent, false);
            return new ChildViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(final RecyclerView.ViewHolder holder, final int position) {
        int itemCount = 0;
        if(mProvinceList.isEmpty()) {
            Logger.w(TAG, "mProvinceList is empty, no data to bind");
            return;
        }
        for (ProvDataInfo provDataInfo : mProvinceList) {
            if(provDataInfo == null) {
                Logger.w(TAG, "provDataInfo is null at position " + position);
                continue;
            }
            if (position == itemCount) {
                if (provDataInfo.isParentCity()) {
                    ((ChildViewHolder) holder).bind(provDataInfo, null);
                } else {
                    ((ParentViewHolder) holder).bind(provDataInfo);
                }
                return;
            }
            itemCount++;
            if (provDataInfo.isExpanded()) {
                if (position < itemCount + provDataInfo.getCityInfoList().size()) {
                    final int childPos = position - itemCount;
                    ((ChildViewHolder) holder).bind(provDataInfo, provDataInfo.getCityInfoList().get(childPos));
                    return;
                }
                itemCount += provDataInfo.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public int getItemCount() {
        if ( !mProvinceList.isEmpty()) {
            int count = mProvinceList.size();
            for (ProvDataInfo parent : mProvinceList) {
                if (parent.isExpanded()) {
                    count += parent.getCityInfoList().size();
                }
            }
            return count;
        } else {
            return 0;
        }
    }

    /**
     * 父项ViewHolder
     */
    class ParentViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView mTitle;
        private SkinImageView mExpandIcon;

        ParentViewHolder(final View itemView) {
            super(itemView);
            mTitle = itemView.findViewById(R.id.item_province_name);
            mExpandIcon = itemView.findViewById(R.id.item_province_expand);

            itemView.setOnClickListener(v -> {
                final int position = getAdapterPosition();
                Logger.d(TAG, "clicking the parent position is " + getParentPosition(position));
                if (position != RecyclerView.NO_POSITION) {
                    final ProvDataInfo parent = mProvinceList.get(getParentPosition(position));
                    if (!parent.isParentCity()) {
                        parent.setExpanded(!parent.isExpanded());
                        notifyDataSetChanged();
                    }
                }
            });
        }

        /**
         * bind
         * @param parent
         */
        void bind(final ProvDataInfo parent) {
            mTitle.setText(parent.getName()); //省份名称
            mExpandIcon.setImageResource(parent.isExpanded() ? R.drawable.img_up_58 : R.drawable.img_under_the_58);
        }
    }

    /**
     * 子项ViewHolder
     */
    class ChildViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView mCityName;
        private SkinTextView mCityData;
        private DownloadBtnView mDownloadBtnView;
        private CustomSwipeMenuLayout mSwipeMenuLayout;
        private SkinButton mAllDownload;
        private SkinButton mAllPause;
        private SkinTextView mDeleteCity;
        private ProgressBar mDownloadProgress;

        ChildViewHolder(final View itemView) {
            super(itemView);
            mCityName = itemView.findViewById(R.id.item_city_name);
            mCityData = itemView.findViewById(R.id.item_city_data);
            mSwipeMenuLayout = itemView.findViewById(R.id.swipe_menu_layout);
            mDownloadBtnView = itemView.findViewById(R.id.item_download_status);
            mAllDownload = itemView.findViewById(R.id.item_all_download);
            mAllPause = itemView.findViewById(R.id.item_all_pause);
            mDeleteCity = itemView.findViewById(R.id.item_driving_delete);
            mDownloadProgress = itemView.findViewById(R.id.download_progress);
        }

        /**
         * bind
         * @param parent
         * @param child
         */
        void bind(final ProvDataInfo parent, final CityDataInfo child) {
            final CityDownLoadInfo downloadItem = parent.isParentCity() ?
                parent.getDownLoadInfo() : child.getDownLoadInfo();
            // 非已下载状态，禁止侧滑删除
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                mSwipeMenuLayout.setSwipeEnabled(true);
            } else {
                mSwipeMenuLayout.setSwipeEnabled(false);
                mSwipeMenuLayout.smoothClose();
            }

            // 城市名称
            mCityName.setText(parent.isParentCity() ? parent.getName() : child.getName());
            // 城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize().longValue());
            mCityData.setText(sizeString);
            if (parent.isParentCity() || !("全省下载").equals(child.getName())) {
                mDownloadBtnView.setVisibility(View.VISIBLE);
                mAllDownload.setVisibility(View.GONE);
                mAllPause.setVisibility(View.GONE);
                //下载按钮状态
                mDownloadBtnView.parseDownloadStatusInfo(downloadItem);
                final boolean isShowDownloadProgress = downloadItem.getTaskState()  == UserDataCode.TASK_STATUS_CODE_DOING
                    || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE
                    || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_UNZIPPING
                    || downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE;
                if (isShowDownloadProgress) {
                    mDownloadProgress.setProgress((int) Math.floor(downloadItem.getPercent()));
                    mDownloadProgress.setVisibility(View.VISIBLE);
                } else {
                    mDownloadProgress.setVisibility(View.GONE);
                }
            } else {
                // 全省条目按钮显示状态
                mDownloadBtnView.setVisibility(View.GONE);
                mAllDownload.setVisibility(View.VISIBLE);
                mAllPause.setVisibility(View.VISIBLE);
                mDownloadProgress.setVisibility(View.GONE);
                final Map<Integer, Integer> allCityTaskStateMap = downloadItem.getAllCityTaskStateMap();
                boolean isAllDownloadEnable = false;
                boolean isAllPauseEnable = false;
                if (allCityTaskStateMap != null) {
                    if (allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_PAUSE)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_ERR)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_READY)) {
                        isAllDownloadEnable = true;
                    }

                    if (allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_DOING)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_DONE)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_WAITING)) {
                        isAllPauseEnable = true;
                    }
                }
                mAllDownload.setEnabled(isAllDownloadEnable);
                mAllPause.setEnabled(isAllPauseEnable);
            }

            // 下载 or 暂停下载
            mDownloadBtnView.setOnClickListener(v -> {
                Logger.d(TAG, "clicking position is " + getAdapterPosition());
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (mOnChildClickListener != null) {
                    switch (downloadItem.getTaskState()) {
                        case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                            mOnChildClickListener.pauseAllTask(cityAdCodes);
                            break;
                        case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                        case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                        case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                        case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                            mOnChildClickListener.startAllTask(cityAdCodes);
                            break;
                        default:
                            break;
                    }
                }
            });
            // 删除已下载数据
            mDeleteCity.setOnClickListener(view -> {
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (mOnChildClickListener != null) {
                    mOnChildClickListener.deleteAllTask(cityAdCodes);
                }
            });
            // 全部开始下载
            mAllDownload.setOnClickListener(view -> {
                final List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                for (CityDataInfo info : cityDataInfos) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX  ) {
                        cityAdCodes.add(info.getAdcode());
                    }
                }
                if (mOnChildClickListener != null) {
                    mOnChildClickListener.allDownloadTask(cityAdCodes);
                }
            });
            // 全部暂停下载
            mAllPause.setOnClickListener(view -> {
                final List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                for (CityDataInfo info : cityDataInfos) {
                    if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                            info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                        cityAdCodes.add(info.getAdcode());
                    }
                }
                if (mOnChildClickListener != null) {
                    mOnChildClickListener.allPauseTask(cityAdCodes);
                }
            });
        }
    }

    /**
     * 更新子项数据
     * @param parentId
     * @param childId
     * @param newValue
     */
    public void updateChild(final int parentId, final int childId, final CityDownLoadInfo newValue) {
        final int childPosition = getChildPosition(parentId, childId);
        Logger.d(TAG, "child position is " + childPosition);
        final int allDownloadPosition = getChildPosition(parentId, parentId);
        Logger.d(TAG, "all download position is " + allDownloadPosition);
        for (ProvDataInfo parent : mProvinceList) {
            if (parent.getAdcode() == parentId) {
                for (CityDataInfo child : parent.getCityInfoList()) {
                    //全省下载条目
                    if (child.getAdcode() == parentId) {
                        final CityDownLoadInfo downLoadInfo = child.getDownLoadInfo();
                        final Map<Integer, Integer> allCityTaskStateMap = downLoadInfo.getAllCityTaskStateMap();
                        if (allCityTaskStateMap != null) {
                            allCityTaskStateMap.put(childId, newValue.getTaskState());
                            if (allDownloadPosition != RecyclerView.NO_POSITION) {
                                notifyItemChanged(allDownloadPosition);
                            }
                        }
                    }

                    if (child.getAdcode() == childId) {
                        child.setDownLoadInfo(newValue);
                        if (childPosition != -1) {
                            notifyItemChanged(childPosition);
                            return;
                        }
                    }
                }
            }
        }
    }

    /**
     * 新增方法：根据父项和子项ID获取子项的绝对位置
     * @param parentId
     * @param childId
     * @return child position
     */
    public int getChildPosition(final int parentId, final int childId) {
        int position = 0;

        for (ProvDataInfo parent : mProvinceList) {
            position++; // 父项位置

            if (parent.getAdcode() == parentId) {
                if (parent.isExpanded()) {
                    for (CityDataInfo child : parent.getCityInfoList()) {
                        if (child.getAdcode() == childId) {
                            return position;
                        }
                        position++;
                    }
                }
                break;
            }

            if (parent.isExpanded()) {
                position += parent.getCityInfoList().size();
            }
        }
        return -1; // 未找到
    }

    /**
     * 辅助方法：根据位置获取父项索引
     * @param adapterPosition
     * @return parent position
     */
    private int getParentPosition(final int adapterPosition) {
        int itemCount = 0;
        int parentIndex = 0;
        for (ProvDataInfo parent : mProvinceList) {
            if (adapterPosition == itemCount) {
                return parentIndex;
            }
            itemCount++;
            if (parent.isExpanded()) {
                if (adapterPosition < itemCount + parent.getCityInfoList().size()) {
                    return parentIndex;
                }
                itemCount += parent.getCityInfoList().size();
            }
            parentIndex++;
        }
        throw new IllegalStateException("Invalid position");
    }

    /**
     * 获取parent city在布局中的位置
     * @param parentId
     * @return parent position
     */
    private int getParentPositionById(final int parentId) {
        int parentPosition = 0;
        for (int i = 0; i < mProvinceList.size(); i++) {
            final ProvDataInfo provDataInfo = mProvinceList.get(i);
            if (provDataInfo.getAdcode() == parentId) {
                return parentPosition;
            } else {
                if (provDataInfo.isExpanded()) {
                    parentPosition += provDataInfo.getCityInfoList().size() + 1;
                } else {
                    parentPosition ++;
                }
            }
        }
        return -1;
    }

    /**
     * 获取Parent项在其所在分组中的相对位置
     * @param parentId 父项ID
     * @return 分组中的索引位置，未找到返回-1
     */
    public int getParentIndex(final int parentId) {
        for (int i = 0; i < mProvinceList.size(); i++) {
            if (mProvinceList.get(i).getAdcode() == parentId) {
                return i;
            }
        }
        return -1;
    }

    /**
     * 更新父项数据并局部刷新
     * @param parentId 父项ID
     * @param info 新ProvDataInfo信息
     */
    public void updateParent(final int parentId, final CityDownLoadInfo info) {
        final int parentIndex = getParentIndex(parentId);
        if (parentIndex != -1) {
           final ProvDataInfo parent = mProvinceList.get(parentIndex);
            parent.setDownLoadInfo(info);
            // 获取父项在列表中的绝对位置
            final int parentPosition = getParentPositionById(parentId);
            Logger.d(TAG, "parent position is " + parentPosition);
            if (parentPosition != -1) {
                notifyItemChanged(parentPosition);
            }
        }
    }

    public interface OnChildClickListener {
        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 取消下载
         */
        void cancelAllTask();
        /**
         * 全部下载
         * @param cityAdCodes
         */
        void allDownloadTask(final ArrayList<Integer> cityAdCodes);
        /**
         * 全部暂停
         * @param cityAdCodes
         */
        void allPauseTask(final ArrayList<Integer> cityAdCodes);
    }

}